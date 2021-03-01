{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Main where

import Control.Category ((>>>))
import Control.Exception (AsyncException (UserInterrupt), IOException, catch, throwIO, try)
import Control.Monad
import Data.Char
import Data.Foldable (for_)
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ANSI qualified as Text
import Data.Text.Encoding.Base64 qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesDirectoryExist, removeFile, withCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (Handle, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Terminal (queryTerminal)
import System.Process
import Text.Read (readMaybe)
import Prelude hiding (head)

-- FIXME: nicer "git status" story. in particular the conflict markers in the commits after a merge are a bit
-- ephemeral feeling
-- FIXME better feedback when exiting due to active merge, or perhaps just do the commit...?
-- FIXME bail if active cherry-pick, active revert, active rebase, what else?
-- FIXME pushing a new branch lists too many commits and doesn't format them
-- FIXME rev-list max 11, use ellipses after 10

-- TODO mit init
-- TODO tweak things to work with git < 2.30.1

main :: IO ()
main = do
  -- TODO fail if not in git repo
  warnIfBuggyGit
  getArgs >>= \case
    ["branch", branch] -> mitBranch (Text.pack branch)
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> mitClone url name
    ["commit"] -> mitCommit
    ["sync"] -> mitSync
    ["undo"] -> mitUndo
    _ ->
      putLines
        [ "Usage:",
          "  mit branch ≪branch≫",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit sync",
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

mitBranch :: Text -> IO ()
mitBranch branch = do
  let upstream :: Text
      upstream =
        "origin/" <> branch

  let worktreeDir :: Text
      worktreeDir =
        Text.dropWhileEnd (/= '/') gitdir <> branch

  unlessM (doesDirectoryExist (Text.unpack worktreeDir)) do
    worktrees :: [(Text, Text, Maybe Text)] <-
      gitWorktreeList

    case List.find (\(_, _, worktreeBranch) -> worktreeBranch == Just branch) worktrees of
      Nothing -> do
        git_ ["worktree", "add", "--detach", "--quiet", worktreeDir]
        withCurrentDirectory (Text.unpack worktreeDir) do
          git ["rev-parse", "--quiet", "--verify", "refs/heads/" <> branch] >>= \case
            False -> do
              git_ ["branch", "--no-track", branch]
              git_ ["switch", "--quiet", branch]

              _fetchResult :: ExitCode <-
                git ["fetch", "--quiet", "origin"]

              whenM (git ["rev-parse", "--quiet", "--verify", "refs/remotes/" <> upstream]) do
                git_ ["reset", "--hard", "--quiet", upstream]
                git_ ["branch", "--set-upstream-to", upstream]
            True ->
              git_ ["switch", "--quiet", branch]
      Just (dir, _, _) ->
        unless (worktreeDir == dir) do
          Text.putStrLn ("Branch " <> Text.bold branch <> " is already checked out in " <> Text.bold dir)
          exitFailure

mitClone :: Text -> Text -> IO ()
mitClone url name =
  -- FIXME use 'git config --get init.defaultBranch
  git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]

mitCommit :: IO ()
mitCommit = do
  whenM gitMergeInProgress exitFailure
  context <- makeContext
  case context.dirty of
    Differences -> mitCommitWith context
    NoDifferences -> mitSyncWith context -- n.b. not covered by tests (would be dupes of sync tests)

mitCommitWith :: Context -> IO ()
mitCommitWith context = do
  stash :: Text <-
    git ["stash", "create"]

  -- FIXME kinda clean this up
  (conflicts1, conflicts2) <-
    case context.remoteCommits of
      [] -> pure (False, False)
      _ -> do
        git_ ["reset", "--hard", "--quiet", "HEAD"]
        git ["merge", "--ff", "--quiet", context.upstream] >>= \case
          False -> do
            git_ ["merge", "--abort"]
            git_ ["stash", "apply", "--quiet", stash]
            pure (True, False)
          True ->
            git ["stash", "apply", "--quiet", stash] >>= \case
              False -> do
                git_ ["reset", "--hard", "--quiet", context.head]
                git_ ["stash", "apply", "--quiet", stash]
                pure (False, True)
              True -> pure (False, False)

  commitResult :: Bool <-
    gitCommit

  (mergeConflicts, stashConflicts) :: ([Text], [Text]) <-
    if conflicts1 || conflicts2
      then do
        unless commitResult (git_ ["reset", "--hard", "--quiet", "HEAD"])
        (,) <$> gitMerge context.upstream <*> gitApplyStash stash
      else pure ([], [])

  localCommits :: [Text] <-
    context.getLocalCommits

  pushResult :: Maybe ExitCode <-
    if not (conflicts1 || conflicts2) && not context.fetchFailed && not (null localCommits)
      then Just <$> gitPush context.branch
      else pure Nothing

  canUndo :: Bool <-
    case (pushResult, commitResult) of
      (Just ExitSuccess, False) -> do
        deleteUndoFile context.branch64
        pure False
      (Just ExitSuccess, True) -> do
        case localCommits of
          [_] -> do
            -- FIXME this call wouldn't be necessary if we don't pretty-print local commits right away
            head <- git ["rev-parse", "HEAD"]
            recordUndoFile context.branch64 [Revert head, Apply stash]
            pure True
          _ -> do
            deleteUndoFile context.branch64
            pure False
      _ -> do
        head <- git ["rev-parse", "HEAD"]
        if head == context.head
          then pure False
          else do
            recordUndoFile context.branch64 [Reset context.head, Apply stash]
            pure True

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        conflicts = List.nub (stashConflicts ++ mergeConflicts),
        localCommits,
        mergeConflicts =
          (conflicts1 || conflicts2)
            && ( if commitResult
                   then conflicts1
                   else not (null mergeConflicts)
               ),
        pushResult,
        remoteCommits = context.remoteCommits,
        upstream = context.upstream
      }

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: IO ()
mitSync = do
  whenM gitMergeInProgress exitFailure
  context <- makeContext
  mitSyncWith context

mitSyncWith :: Context -> IO ()
mitSyncWith context = do
  maybeStash :: Maybe Text <-
    case (context.remoteCommits, context.dirty) of
      (_ : _, Differences) -> Just <$> gitStash
      _ -> pure Nothing

  mergeConflicts :: Maybe [Text] <-
    case context.remoteCommits of
      [] -> pure Nothing
      _ -> Just <$> gitMerge context.upstream

  localCommits :: [Text] <-
    context.getLocalCommits

  stashConflicts :: [Text] <-
    case maybeStash of
      Nothing -> pure []
      Just stash -> gitApplyStash stash

  let conflicts :: [Text]
      conflicts =
        List.nub (stashConflicts ++ fromMaybe [] mergeConflicts)

  pushResult :: Maybe ExitCode <-
    if not context.fetchFailed && not (null localCommits) && null conflicts
      then Just <$> gitPush context.branch
      else pure Nothing

  canUndo :: Bool <-
    case pushResult of
      Just ExitSuccess -> do
        deleteUndoFile context.branch64
        pure False
      _ -> do
        head2 <- git ["rev-parse", "HEAD"]
        if context.head == head2
          then pure False -- head didn't move; nothing to undo
          else do
            recordUndoFile context.branch64 (Reset context.head : maybeToList (Apply <$> maybeStash))
            pure True

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        conflicts,
        localCommits,
        mergeConflicts = maybe False (not . null) mergeConflicts,
        pushResult,
        remoteCommits = context.remoteCommits,
        upstream = context.upstream
      }

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
          deleteUndoFile branch64
          for_ undos \case
            Apply commit -> git_ ["stash", "apply", "--quiet", commit]
            Reset commit -> git_ ["reset", "--hard", "--quiet", commit]
            Revert commit -> git_ ["revert", commit]
          when (undosContainRevert undos) mitSync
  where
    undosContainRevert :: [Undo] -> Bool
    undosContainRevert = \case
      [] -> False
      Revert _ : _ -> True
      _ : undos -> undosContainRevert undos

-- FIXME add mkCommitsAhead
data Context = Context
  { branch :: Text,
    branch64 :: Text,
    dirty :: DiffResult,
    fetchFailed :: Bool,
    getLocalCommits :: IO [Text],
    head :: Text,
    maybeUpstreamHead :: Maybe Text,
    remoteCommits :: [Text],
    upstream :: Text
  }

makeContext :: IO Context
makeContext = do
  branch :: Text <-
    gitCurrentBranch

  let upstream :: Text
      upstream =
        "origin/" <> branch

  dirty :: DiffResult <-
    gitDiff

  head :: Text <-
    git ["rev-parse", "HEAD"]

  fetchFailed :: Bool <-
    not <$> git ["fetch", "--quiet", "origin"]

  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  let getLocalCommits :: IO [Text]
      getLocalCommits =
        case maybeUpstreamHead of
          Nothing -> git ["rev-list", "HEAD"] -- FIXME
          Just upstreamHead -> prettyCommitsBetween upstreamHead "HEAD"

  remoteCommits :: [Text] <-
    case maybeUpstreamHead of
      Nothing -> pure []
      Just upstreamHead -> prettyCommitsBetween head upstreamHead

  pure
    Context
      { branch,
        branch64 = Text.encodeBase64 branch,
        dirty,
        fetchFailed,
        getLocalCommits,
        head,
        maybeUpstreamHead,
        remoteCommits,
        upstream
      }

data Summary = Summary
  { branch :: Text,
    canUndo :: Bool,
    conflicts :: [Text],
    localCommits :: [Text],
    mergeConflicts :: Bool,
    pushResult :: Maybe ExitCode,
    remoteCommits :: [Text],
    upstream :: Text
  }

-- FIXME show some graph of where local/remote is at
putSummary :: Summary -> IO ()
putSummary summary = do
  putLines . concatMap ("" :) . catMaybes $
    [ do
        guard (not (null summary.remoteCommits))
        let colorize = if summary.mergeConflicts then Text.red else Text.green
        Just
          ( colorize (Text.italic ("  " <> summary.upstream <> " → " <> summary.branch)) :
            map ("  " <>) summary.remoteCommits
          ),
      do
        guard (not (null summary.localCommits))
        let colorize =
              case summary.pushResult of
                Just ExitSuccess -> Text.green
                _ -> Text.red
        Just
          ( colorize (Text.italic ("  " <> summary.branch <> " → " <> summary.upstream)) :
            map ("  " <>) summary.localCommits
          ),
      do
        guard (not (null summary.conflicts))
        Just ("  The following files have conflicts." : map (("    " <>) . Text.red) summary.conflicts),
      do
        guard summary.canUndo
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

-- | Apply stash, return conflicts.
gitApplyStash :: Text -> IO [Text]
gitApplyStash stash = do
  git ["stash", "apply", "--quiet", stash] >>= \case
    False -> do
      conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      git_ ["reset", "--quiet"] -- unmerged (weird) -> unstaged (normal)
      pure conflicts
    True -> pure []

gitCommit :: IO Bool
gitCommit =
  queryTerminal 0 >>= \case
    False -> do
      message <- lookupEnv "MIT_COMMIT_MESSAGE"
      git ["commit", "--all", "--message", maybe "" Text.pack message, "--quiet"]
    True ->
      git2 ["commit", "--patch", "--quiet"] <&> \case
        ExitFailure _ -> False
        ExitSuccess -> True

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
    False -> Differences
    True -> NoDifferences

-- FIXME document what this does
gitMerge :: Text -> IO [Text]
gitMerge target = do
  git ["merge", "--ff", "--quiet", target] >>= \case
    False -> do
      conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      git_ ["add", "--all"]
      -- TODO better commit message than the default
      git_ ["commit", "--no-edit", "--quiet"]
      pure conflicts
    True -> pure []

gitMergeInProgress :: IO Bool
gitMergeInProgress =
  not <$> git ["merge", "--quiet", "HEAD"]

gitPush :: Text -> IO ExitCode
gitPush branch =
  git ["push", "--quiet", "--set-upstream", "origin", branch <> ":" <> branch]

data GitRevisionInfo = GitRevisionInfo
  { author :: Text,
    date :: Text,
    hash :: Text,
    subject :: Text
  }

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

-- /dir/one 0efd393c35 [oingo]         -> ("/dir/one", "0efd393c35", Just "oingo")
-- /dir/two dc0c114266 (detached HEAD) -> ("/dir/two", "dc0c114266", Nothing)
gitWorktreeList :: IO [(Text, Text, Maybe Text)]
gitWorktreeList = do
  map f <$> git ["worktree", "list"]
  where
    f :: Text -> (Text, Text, Maybe Text)
    f line =
      case Text.words line of
        [dir, commit, stripBrackets -> Just branch] -> (dir, commit, Just branch)
        [dir, commit, "(detached", "HEAD)"] -> (dir, commit, Nothing)
        _ -> error (Text.unpack line)
      where
        stripBrackets :: Text -> Maybe Text
        stripBrackets =
          Text.stripPrefix "[" >=> Text.stripSuffix "]"

debug :: Bool
debug =
  isJust (unsafePerformIO (lookupEnv "debug"))
{-# NOINLINE debug #-}

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

instance ProcessOutput Bool where
  fromProcessOutput _ _ = \case
    ExitFailure _ -> pure False
    ExitSuccess -> pure True

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

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mx action =
  mx >>= \case
    False -> action
    True -> pure ()

whenM :: Monad m => m Bool -> m () -> m ()
whenM mx action =
  mx >>= \case
    False -> pure ()
    True -> action
