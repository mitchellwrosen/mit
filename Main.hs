{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Category ((>>>))
import Control.Exception (AsyncException (UserInterrupt), IOException, catch, throwIO, try)
import Control.Monad
import Data.Char
import Data.Foldable (for_)
import Data.Function
import Data.IORef
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ANSI qualified as Text
import Data.Text.Encoding.Base64 qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (removeFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (Handle, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Read (readMaybe)
import Prelude hiding (head)

-- FIXME show commit summary
-- FIXME: nicer "git status" story. in particular the conflict markers in the commits after a merge are a bit
-- ephemeral feeling

main :: IO ()
main = do
  -- TODO fail if not in git repo
  warnIfBuggyGit
  getArgs >>= \case
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] ->
      git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"] >>= \case
        ExitFailure _ -> exitFailure
        ExitSuccess -> pure ()
    ["commit"] -> mitCommit
    ["sync"] -> mitSync
    ["undo"] -> mitUndo
    _ ->
      putLines
        [ "Usage:",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit sync [≪branch≫]",
          "  mit undo"
        ]
  where
    warnIfBuggyGit :: IO ()
    warnIfBuggyGit = do
      version <- gitVersion
      case foldr (\(ver, warn) acc -> if version < ver then (ver, warn) : acc else acc) [] validations of
        [] -> pure ()
        warnings ->
          putLines $
            map
              ( \(ver, warn) ->
                  Text.yellow ("Prior to " <> Text.bold "git" <> " version " <> showGitVersion ver <> ", " <> warn)
              )
              warnings
      where
        validations :: [(GitVersion, Text)]
        validations =
          [ ( GitVersion 2 29 0,
              Text.bold "git commit --patch"
                <> " was broken for new files added with "
                <> Text.bold "git add --intent-to-add"
                <> "."
            ),
            ( GitVersion 2 30 1,
              Text.bold "git stash create"
                <> " was broken for new files added with "
                <> Text.bold "git add --intent-to-add"
                <> "."
            )
          ]

-- FIXME output what we just undid
mitUndo :: IO ()
mitUndo = do
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  let file = undofile branch64
  try (Text.readFile file) >>= \case
    Left (_ :: IOException) -> exitFailure
    Right contents ->
      case parseUndos contents of
        Nothing -> throwIO (userError ("Corrupt undo file: " ++ file))
        Just undos -> do
          for_ undos \case
            Apply commit -> git_ ["stash", "apply", "--quiet", commit]
            Reset commit -> git_ ["reset", "--hard", "--quiet", commit]
            Revert commit -> git_ ["revert", commit]
          -- TODO if we reverted, we also want to push
          deleteUndoFile branch64

mitCommit :: IO ()
mitCommit = do
  -- FIXME better feedback, or perhaps just do the commit...?
  whenM gitMergeInProgress exitFailure
  gitDiff >>= \case
    Differences -> pure ()
    -- TODO do some sort of "mit status" here?
    NoDifferences -> exitFailure
  gitFetch
  branch <- gitCurrentBranch
  let branch64 :: Text
      branch64 =
        Text.encodeBase64 branch
  let upstream :: Text
      upstream =
        "origin/" <> branch
  let isUpstreamAhead :: IO Bool
      isUpstreamAhead = do
        git ["show-ref", "--quiet", "--verify", "refs/remotes/" <> upstream] >>= \case
          ExitFailure _ -> pure False
          ExitSuccess -> do
            commits <- git ["rev-list", "--max-count", "1", branch <> ".." <> upstream]
            pure (not (null (commits :: [Text])))
  head <- git ["rev-parse", "HEAD"]
  isUpstreamAhead >>= \case
    False -> do
      stash <- git ["stash", "create"]
      git2 ["commit", "--patch", "--quiet"] >>= \case
        ExitFailure _ -> exitFailure
        ExitSuccess -> do
          recordUndoFile branch64 [Reset head, Apply stash]
          -- FIXME report status/feedback on push fail
          gitPush branch >>= \case
            ExitFailure _ -> pure () -- FIXME report this
            ExitSuccess -> do
              head2 <- git ["rev-parse", "HEAD"]
              recordUndoFile branch64 [Revert head2, Apply stash]
    True -> do
      stash <- gitStash
      let -- Handle the case that our commit conflicts with upstream, possibly because we were *already* forked.
          -- We want to make our commit locally, then merge in the conflicts and commit that, then report on what just
          -- happened. From there, the user can undo the whole operation, or else address the conflict markers and
          -- commit again.
          --
          -- FIXME: if commit is bailed, report on the conflicts that we know exist?
          commitThenMergeInConflicts :: IO ()
          commitThenMergeInConflicts = do
            git2 ["commit", "--patch", "--quiet"] >>= \case
              ExitFailure _ -> exitFailure
              ExitSuccess -> do
                head2 <- git ["rev-parse", "HEAD"]
                gitMerge upstream >>= \case
                  MergeFailed conflicts -> do
                    recordUndoFile branch64 [Reset head, Apply stash]
                    head3 <- git ["rev-parse", "HEAD"]
                    commits2 <- prettyCommitsBetween head2 (head3 <> "^2")
                    putLines (syncMessage upstream commits2 Nothing conflicts)
                  -- Impossible: we just observed a conflict when popping the stash. Then we backed out, applied the
                  -- stash to pre-merge HEAD, and merged again to get the same conflicts, only with the uncommitted
                  -- changes actually committed.
                  MergeSucceeded -> undefined
      git ["merge", "--ff", "--quiet", upstream] >>= \case
        ExitFailure _ -> do
          git_ ["merge", "--abort"]
          git_ ["stash", "apply", "--quiet", stash]
          commitThenMergeInConflicts
        ExitSuccess -> do
          head2 <- git ["rev-parse", "HEAD"]
          commits <- prettyCommitsBetween head head2
          gitApplyStash stash >>= \case
            [] -> do
              git2 ["commit", "--patch", "--quiet"] >>= \case
                ExitFailure _ -> do
                  recordUndoFile branch64 [Reset head, Apply stash]
                  putLines (syncMessage upstream commits Nothing [])
                ExitSuccess -> do
                  recordUndoFile branch64 [Reset head, Apply stash]
                  -- FIXME add "commit" to summary
                  gitPush branch >>= \case
                    ExitFailure _ -> putLines (syncMessage upstream commits Nothing [])
                    ExitSuccess -> do
                      head3 <- git ["rev-parse", "HEAD"]
                      recordUndoFile branch64 [Revert head3, Apply stash]
                      putLines (syncMessage upstream commits Nothing [])
            _conflicts -> do
              git_ ["reset", "--hard", "--quiet", head]
              git_ ["stash", "apply", "--quiet", stash]
              commitThenMergeInConflicts

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: IO ()
mitSync = do
  -- FIXME better feedback, or perhaps just do the commit...?
  whenM gitMergeInProgress exitFailure

  branch :: Text <-
    gitCurrentBranch

  let branch64 :: Text
      branch64 =
        Text.encodeBase64 branch

  let upstream :: Text
      upstream =
        "origin/" <> branch

  head :: Text <-
    git ["rev-parse", "HEAD"]

  fetchFailed :: Bool <-
    git ["fetch", "--quiet", "origin"] <&> \case
      ExitFailure _ -> True
      ExitSuccess -> False

  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  commitsToMerge :: [Text] <-
    case maybeUpstreamHead of
      Nothing -> pure []
      Just upstreamHead -> prettyCommitsBetween head upstreamHead

  maybeStash :: Maybe Text <-
    case commitsToMerge of
      [] -> pure Nothing
      _ ->
        gitDiff >>= \case
          Differences -> Just <$> gitStash
          NoDifferences -> pure Nothing

  mergeConflicts :: Maybe [Text] <-
    case commitsToMerge of
      [] -> pure Nothing
      _ ->
        gitMerge upstream <&> \case
          MergeFailed conflicts -> Just conflicts
          MergeSucceeded -> Just []

  commitsAhead :: [Text] <-
    case maybeUpstreamHead of
      Nothing -> git ["rev-list", "HEAD"]
      Just upstreamHead ->
        prettyCommitsBetween
          upstreamHead
          -- catch the merge bubble
          (if isNothing mergeConflicts then head else "HEAD")

  stashConflicts :: [Text] <-
    case maybeStash of
      Nothing -> pure []
      Just stash -> gitApplyStash stash

  let conflicts :: [Text]
      conflicts =
        List.nub (stashConflicts ++ fromMaybe [] mergeConflicts)

  pushResult :: Maybe ExitCode <-
    if not fetchFailed && not (null commitsAhead) && null conflicts
      then Just <$> gitPush branch
      else pure Nothing

  canUndo :: Bool <-
    case pushResult of
      -- During a 'mit sync', if we pushed, we do not want to allow undo by reverting the commit(s), because it'd be
      -- difficult to undo the undo (sometimes it requires reverting the revert, or some other fanciness that is for
      -- sure possible with git, but which we don't have a good UX for).
      --
      -- The difference with 'mit commit' is when you revert a commit you just pushed, you at least have the local
      -- changes in your working tree, to easily be committed again. These can just be re-applied with a second
      -- identical commit, i.e. 'mit commit && mit undo && mit commit' works fine.
      Just ExitSuccess -> do
        deleteUndoFile branch64
        pure False
      _ -> do
        head2 <- git ["rev-parse", "HEAD"]
        if head == head2
          then pure False -- head didn't move; nothing to undo
          else do
            recordUndoFile branch64 (Reset head : maybeToList (Apply <$> maybeStash))
            pure True

  -- FIXME show some graph of where local/remote is at

  putLines . concatMap ("" :) . catMaybes $
    [ do
        guard (not (null commitsToMerge))
        let colorize = if null (fromMaybe [] mergeConflicts) then Text.green else Text.red
        Just (colorize (Text.italic ("  " <> branch <> " ← " <> upstream)) : map ("  " <>) commitsToMerge),
      do
        guard (not (null commitsAhead))
        let colorize =
              case pushResult of
                Just ExitSuccess -> Text.green
                _ -> Text.red
        Just (colorize (Text.italic ("  " <> branch <> " → " <> upstream)) : map ("  " <>) commitsAhead),
      do
        guard (not (null conflicts))
        Just ("  The following files have conflicts." : map (("    " <>) . Text.red) conflicts),
      do
        guard canUndo
        Just ["  Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change."]
    ]

deleteUndoFile :: Text -> IO ()
deleteUndoFile branch64 =
  removeFile (undofile branch64) `catch` \(_ :: IOException) -> pure ()

gitdir :: Text
gitdir =
  unsafePerformIO (git ["rev-parse", "--absolute-git-dir"])
{-# NOINLINE gitdir #-}

undofile :: Text -> FilePath
undofile branch64 =
  Text.unpack (gitdir <> "/mit-" <> branch64)

data Undo
  = Apply Text -- apply stash
  | Reset Text -- reset to commit
  | Revert Text -- revert commit

showUndos :: [Undo] -> Text
showUndos =
  Text.intercalate "," . map showUndo
  where
    showUndo :: Undo -> Text
    showUndo = \case
      Apply commit -> "apply " <> commit
      Reset commit -> "reset " <> commit
      Revert commit -> "revert " <> commit

parseUndos :: Text -> Maybe [Undo]
parseUndos =
  Text.split (== ',') >>> traverse parseUndo
  where
    parseUndo :: Text -> Maybe Undo
    parseUndo =
      Text.words >>> \case
        ["apply", commit] -> Just (Apply commit)
        ["reset", commit] -> Just (Reset commit)
        ["revert", commit] -> Just (Revert commit)
        _ -> Nothing

-- Record a file for 'mit undo' to use, if invoked.
recordUndoFile :: Text -> [Undo] -> IO ()
recordUndoFile branch64 undos = do
  Text.writeFile (undofile branch64) (showUndos undos)

data Summary = Summary
  { localCommitsApplied :: [Text], -- applied but not published
    localCommitsPublished :: [Text], -- applied but not published
    remoteCommitsApplied :: [Text]
  }

-- FIXME prettify
summaryMessage :: Summary -> [Text]
summaryMessage Summary {localCommitsApplied, localCommitsPublished, remoteCommitsApplied} =
  concat
    [ "local commits applied" : localCommitsApplied,
      "local commits published" : localCommitsPublished,
      "remote commits applied" : remoteCommitsApplied
    ]

putSummary :: Summary -> IO ()
putSummary =
  putLines . summaryMessage

-- FIXME rename
syncMessage :: Text -> [Text] -> Maybe Text -> [Text] -> [Text]
syncMessage target remoteCommits maybeCommit conflicts =
  remoteCommitsMessage
    & appendCommitMessage
    & appendConflictsMessage
    & (++ ["", undoMessage])
  where
    remoteCommitsMessage :: [Text]
    remoteCommitsMessage =
      "Synchronized with " <> Text.italic target <> "." :
      "" :
      map ("  " <>) remoteCommits
    appendCommitMessage :: [Text] -> [Text]
    appendCommitMessage =
      case maybeCommit of
        Nothing -> id
        Just commit -> (++ ["", "Made a cool commit: " <> commit])
    appendConflictsMessage :: [Text] -> [Text]
    appendConflictsMessage =
      if null conflicts
        then id
        else
          ( ++
              ( "" :
                "The following files have conflicts." :
                "" :
                map (("  " <>) . Text.red) conflicts
              )
          )
    undoMessage :: Text
    undoMessage =
      "Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change."

-- | Apply stash, return conflicts.
-- FIXME return action that returns conflicts instead, since they aren't always used
gitApplyStash :: Text -> IO [Text]
gitApplyStash stash = do
  git ["stash", "apply", "--quiet", stash] >>= \case
    ExitFailure _ -> do
      conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      git_ ["reset", "--quiet"] -- unmerged (weird) -> unstaged (normal)
      pure conflicts
    ExitSuccess -> pure []

-- | Get the current branch.
gitCurrentBranch :: IO Text
gitCurrentBranch =
  git ["branch", "--show-current"]

-- | Create a stash and blow away local changes (like 'git stash push')
gitStash :: IO Text
gitStash = do
  stash <- git ["stash", "create"]
  git_ ["reset", "--hard", "--quiet", "HEAD"]
  pure stash

data DiffResult
  = Differences
  | NoDifferences

gitDiff :: IO DiffResult
gitDiff = do
  git_ ["reset", "--quiet"]
  git_ ["add", "--all", "--intent-to-add"]
  git ["diff", "--quiet"] <&> \case
    ExitFailure _ -> Differences
    ExitSuccess -> NoDifferences

-- | git fetch origin, at most once per run.
gitFetch :: IO ()
gitFetch = do
  fetched <- readIORef fetchedRef
  unless fetched do
    git_ ["fetch", "--quiet", "origin"]
    writeIORef fetchedRef True

fetchedRef :: IORef Bool
fetchedRef =
  unsafePerformIO (newIORef False)
{-# NOINLINE fetchedRef #-}

data MergeResult
  = MergeFailed [Text]
  | MergeSucceeded

-- FIXME document
gitMerge :: Text -> IO MergeResult
gitMerge target = do
  git ["merge", "--ff", "--quiet", target] >>= \case
    ExitFailure _ -> do
      conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      git_ ["add", "--all"]
      -- TODO better commit message than the default
      git_ ["commit", "--no-edit", "--quiet"]
      pure (MergeFailed conflicts)
    ExitSuccess -> pure MergeSucceeded

gitMergeInProgress :: IO Bool
gitMergeInProgress =
  git ["merge", "--quiet", "HEAD"] <&> \case
    ExitFailure _ -> True
    ExitSuccess -> False

gitPush :: Text -> IO ExitCode
gitPush branch =
  git ["push", "--quiet", "--set-upstream", "origin", branch <> ":" <> branch]

data GitRevisionInfo = GitRevisionInfo
  { author :: Text,
    date :: Text,
    hash :: Text,
    subject :: Text
  }

existCommitsBetween :: Text -> Text -> IO Bool
existCommitsBetween commit1 commit2 = do
  commits <- git ["rev-list", "--max-count", "1", commit1 <> ".." <> commit2]
  pure (not (null @[] commits))

prettyCommitsBetween :: Text -> Text -> IO [Text]
prettyCommitsBetween commit1 commit2 =
  if commit1 == commit2
    then pure []
    else do
      commits <-
        -- --first-parent seems desirable for topic branches
        git
          [ "rev-list",
            "--color=always",
            "--date=human",
            "--format=format:%C(bold black)%h%C(reset) %C(bold white)%s%C(reset) – %C(italic white)%an%C(reset) %C(italic yellow)%ad%C(reset)",
            "--max-count=10",
            commit1 <> ".." <> commit2
          ]
      pure (dropEvens commits)
  where
    -- git rev-list with a custom format prefixes every commit with a redundant line :|
    dropEvens :: [a] -> [a]
    dropEvens = \case
      _ : x : xs -> x : dropEvens xs
      xs -> xs

data GitVersion
  = GitVersion Int Int Int
  deriving stock (Eq, Ord)

gitVersion :: IO GitVersion
gitVersion = do
  v0 <- git ["--version"]
  fromMaybe (throwIO (userError ("Could not parse git version from: " <> Text.unpack v0))) do
    ["git", "version", v1] <- Just (Text.words v0)
    [sx, sy, sz] <- Just (Text.split (== '.') v1)
    x <- readMaybe (Text.unpack sx)
    y <- readMaybe (Text.unpack sy)
    z <- readMaybe (Text.unpack sz)
    pure (pure (GitVersion x y z))

showGitVersion :: GitVersion -> Text
showGitVersion (GitVersion x y z) =
  Text.pack (show x) <> "." <> Text.pack (show y) <> "." <> Text.pack (show z)

debug :: Bool
debug =
  isJust (unsafePerformIO (lookupEnv "debug"))
{-# NOINLINE debug #-}

die :: Text -> IO a
die message = do
  Text.putStrLn message
  exitFailure

-- git@github.com:mitchellwrosen/mit.git -> Just ("git@github.com:mitchellwrosen/mit.git", "mit")
parseGitRepo :: Text -> Maybe (Text, Text)
parseGitRepo url = do
  url' <- Text.stripSuffix ".git" url
  pure (url, Text.takeWhileEnd (/= '/') url')

-- Some ad-hoc process return value overloading, for cleaner syntax

class ProcessOutput a where
  fromProcessOutput :: [Text] -> [Text] -> ExitCode -> IO a

instance ProcessOutput () where
  fromProcessOutput _ _ code =
    when (code /= ExitSuccess) (exitWith code)

instance ProcessOutput ExitCode where
  fromProcessOutput _ _ = pure

instance ProcessOutput Text where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    case out of
      [] -> throwIO (userError "no stdout")
      line : _ -> pure line

instance a ~ Text => ProcessOutput [a] where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    pure out

instance a ~ ExitCode => ProcessOutput (Either a Text) where
  fromProcessOutput out _ code =
    case code of
      ExitFailure _ -> pure (Left code)
      ExitSuccess ->
        case out of
          [] -> throwIO (userError "no stdout")
          line : _ -> pure (Right line)

--

git :: ProcessOutput a => [Text] -> IO a
git args = do
  (Nothing, Just stdoutHandle, Just stderrHandle, processHandle) <-
    createProcess
      CreateProcess
        { child_group = Nothing,
          child_user = Nothing,
          close_fds = True,
          cmdspec = RawCommand "git" (map Text.unpack args),
          create_group = False,
          cwd = Nothing,
          delegate_ctlc = False,
          env = Nothing,
          new_session = False,
          std_err = CreatePipe,
          std_in = NoStream,
          std_out = CreatePipe,
          -- windows-only
          create_new_console = False,
          detach_console = False,
          use_process_jobs = False
        }
  exitCode <- waitForProcess processHandle
  stdoutLines <- drainTextHandle stdoutHandle
  stderrLines <- drainTextHandle stderrHandle
  when debug do
    Text.putStrLn . Text.brightBlack $
      Text.unwords ("git" : map quoteText args)
        <> " : "
        <> Text.pack (show (stdoutLines, stderrLines, exitCode))
  fromProcessOutput stdoutLines stderrLines exitCode

git_ :: [Text] -> IO ()
git_ =
  git

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
git2 :: [Text] -> IO ExitCode
git2 args = do
  when debug do
    let quote :: Text -> Text
        quote s =
          if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s
    Text.putStrLn (Text.brightBlack (Text.unwords ("git" : map quote args)))
  (Nothing, Nothing, Just stderrHandle, processHandle) <-
    createProcess
      CreateProcess
        { child_group = Nothing,
          child_user = Nothing,
          close_fds = True,
          cmdspec = RawCommand "git" (map Text.unpack args),
          create_group = False,
          cwd = Nothing,
          delegate_ctlc = True,
          env = Nothing,
          new_session = False,
          std_err = CreatePipe,
          std_in = Inherit,
          std_out = Inherit,
          -- windows-only
          create_new_console = False,
          detach_console = False,
          use_process_jobs = False
        }
  exitCode <-
    waitForProcess processHandle `catch` \case
      UserInterrupt -> pure (ExitFailure (-130))
      exception -> throwIO exception
  stderrLines <- drainTextHandle stderrHandle
  when debug do
    Text.putStrLn . Text.brightBlack $
      Text.unwords ("git" : map quoteText args) <> " : " <> Text.pack (show (stderrLines, exitCode))
  pure exitCode

--

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip fmap

drainTextHandle :: Handle -> IO [Text]
drainTextHandle handle = do
  let loop acc =
        hIsEOF handle >>= \case
          False -> do
            line <- Text.hGetLine handle
            loop (line : acc)
          True -> pure (reverse acc)
  loop []

putLines :: [Text] -> IO ()
putLines =
  Text.putStr . Text.unlines

quoteText :: Text -> Text
quoteText s =
  if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s

whenM :: Monad m => m Bool -> m () -> m ()
whenM mx action =
  mx >>= \case
    False -> pure ()
    True -> action
