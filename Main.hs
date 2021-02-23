module Main where

import Control.Exception (AsyncException (UserInterrupt), IOException, catch, throwIO, try)
import Control.Monad
import Data.Char
import Data.IORef
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ANSI qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (removeFile, renameFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (Handle, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Read (readMaybe)
import Prelude hiding (head)

-- FIXME show commit summary

main :: IO ()
main = do
  -- TODO fail if not in git repo
  warnIfBuggyGit
  getArgs >>= \case
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> do
      code <- git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]
      when (code /= ExitSuccess) (exitWith code)
    ["commit"] -> mitCommit
    ["sync"] -> mitSync Nothing
    ["sync", branch] -> mitSync (Just (Text.pack branch))
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
  Stdout head <- git ["rev-parse", "HEAD"]
  let file = undofile head
  try (Text.readFile file) >>= \case
    Left (_ :: IOException) -> exitFailure
    Right contents ->
      case Text.words contents of
        [previousHead] -> do
          () <- git ["reset", "--hard", "--quiet", previousHead]
          deleteUndoFile head
        [previousHead, stash] -> do
          () <- git ["reset", "--hard", "--quiet", previousHead]
          () <- git ["stash", "apply", "--quiet", stash]
          deleteUndoFile head
        _ -> exitFailure

-- FIXME double-check undo file of HEAD prior to successful commit is deleted
-- (rather, set to revert if push succeeds, otherwise updated to latest)
mitCommit :: IO ()
mitCommit = do
  git ["merge", "--quiet", "HEAD"] >>= \case
    ExitFailure _ -> exitFailure -- FIXME better feedback, or perhaps just do the commit.
    ExitSuccess -> pure ()
  gitDiff >>= \case
    Differences -> pure ()
    -- TODO do some sort of "mit status" here?
    NoDifferences -> exitFailure
  gitFetch
  Stdout branch <- git ["branch", "--show-current"]
  let upstream :: Text
      upstream =
        "origin/" <> branch
  let isUpstreamAhead :: IO Bool
      isUpstreamAhead = do
        git ["show-ref", "--quiet", "--verify", "refs/remotes/" <> upstream] >>= \case
          ExitFailure _ -> pure False
          ExitSuccess -> do
            Stdout commits <- git ["rev-list", "--max-count", "1", branch <> ".." <> upstream]
            pure (not (null (commits :: [Text])))
  Stdout head <- git ["rev-parse", "HEAD"]
  isUpstreamAhead >>= \case
    False -> do
      git2 ["commit", "--patch", "--quiet"] >>= \case
        ExitFailure _ -> exitFailure
        ExitSuccess -> do
          -- FIXME report status and make 'mit undo' work
          git ["push", "--quiet", "--set-upstream", "origin", branch <> ":" <> branch]
    True -> do
      stash <- gitCreateStash
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
                Stdout head2 <- git ["rev-parse", "HEAD"]
                gitMerge upstream >>= \case
                  MergeFailed conflicts -> do
                    head3 <- recordUndoFile head (Just stash)
                    commits2 <- prettyCommitsBetween head2 (head3 <> "^2")
                    putLines (syncMessage upstream commits2 conflicts)
                  -- Impossible: we just observed a conflict when popping the stash. Then we backed out, applied the
                  -- stash to pre-merge HEAD, and merged again to get the same conflicts, only with the uncommitted
                  -- changes actually committed.
                  MergeSucceeded -> undefined
      git ["merge", "--ff", "--quiet", upstream] >>= \case
        ExitFailure _ -> do
          () <- git ["merge", "--abort"]
          () <- git ["stash", "apply", "--quiet", stash]
          commitThenMergeInConflicts
        ExitSuccess -> do
          Stdout head2 <- git ["rev-parse", "HEAD"]
          commits <- prettyCommitsBetween head head2
          gitApplyStash stash >>= \case
            [] -> do
              git2 ["commit", "--patch", "--quiet"] >>= \case
                ExitFailure _ -> do
                  _ <- recordUndoFile head (Just stash)
                  putLines (syncMessage upstream commits [])
                ExitSuccess -> do
                  head3 <- recordUndoFile head (Just stash)
                  -- FIXME add "commit" to summary
                  result <- git ["push", "--quiet", "--set-upstream", "origin", branch <> ":" <> branch]
                  -- delay this until after the push because it looks weird to get feedback, then hang for a sec
                  putLines (syncMessage upstream commits [])
                  case result of
                    ExitFailure _ -> pure ()
                    -- FIXME set new "undo" to revert somehow
                    ExitSuccess -> deleteUndoFile head3
            _conflicts -> do
              () <- git ["reset", "--hard", "--quiet", head]
              () <- git ["stash", "apply", "--quiet", stash]
              commitThenMergeInConflicts

-- FIXME: check if 'mit sync' pushes unpublished changes. seems like it should.
mitSync :: Maybe Text -> IO ()
mitSync maybeBranch = do
  git ["merge", "--quiet", "HEAD"] >>= \case
    ExitFailure _ -> exitFailure
    ExitSuccess -> pure ()
  gitFetch
  (target, targetHash :: Text) <- do
    branch <-
      case maybeBranch of
        Nothing -> do
          Stdout branch <- git ["branch", "--show-current"]
          pure branch
        Just branch -> pure branch
    git ["rev-parse", "refs/remotes/origin/" <> branch] >>= \case
      Left _ -> do
        when (isNothing maybeBranch) exitSuccess -- sync with self
        git ["rev-parse", branch] >>= \case
          Left _ -> exitFailure -- sync with non-existent
          Right (Stdout targetHash) -> pure (branch, targetHash)
      Right (Stdout targetHash) -> pure ("origin/" <> branch, targetHash)
  gitDiff >>= \case
    Differences -> do
      Stdout head <- git ["rev-parse", "HEAD"]
      unless (head == targetHash) do
        stash <- gitCreateStash
        gitMerge target >>= \case
          MergeFailed mergeConflicts -> do
            head2 <- recordUndoFile head (Just stash)
            commits <- prettyCommitsBetween head (head2 <> "^2")
            stashConflicts <- gitApplyStash stash
            putLines (syncMessage target commits (List.nub (stashConflicts ++ mergeConflicts)))
            exitFailure
          MergeSucceeded -> do
            Stdout head2 <- git ["rev-parse", "HEAD"]
            commits <- prettyCommitsBetween head head2
            _ <- recordUndoFile head (Just stash)
            conflicts <- gitApplyStash stash
            putLines (syncMessage target commits conflicts)
    NoDifferences -> do
      Stdout head <- git ["rev-parse", "HEAD"]
      unless (head == targetHash) do
        gitMerge target >>= \case
          -- FIXME: nicer "git status" story here. the fact that there are conflict markers isn't obvious
          MergeFailed conflicts -> do
            head2 <- recordUndoFile head Nothing
            commits <- prettyCommitsBetween head (head2 <> "^2")
            putLines (syncMessage target commits conflicts)
            exitFailure
          MergeSucceeded -> do
            Stdout head2 <- git ["rev-parse", "HEAD"]
            commits <- prettyCommitsBetween head head2
            _ <- recordUndoFile head Nothing
            putLines (syncMessage target commits [])

deleteUndoFile :: Text -> IO ()
deleteUndoFile head =
  removeFile (undofile head) `catch` \(_ :: IOException) -> pure ()

renameUndoFile :: Text -> Text -> IO ()
renameUndoFile previousHead head =
  renameFile (undofile previousHead) (undofile head) `catch` \(_ :: IOException) -> pure ()

gitdir :: Text
gitdir =
  unsafePerformIO do
    Stdout dir <- git ["rev-parse", "--absolute-git-dir"]
    pure dir
{-# NOINLINE gitdir #-}

undofile :: Text -> FilePath
undofile head =
  Text.unpack (gitdir <> "/mit-" <> head)

-- Record a file for 'mit undo' to use, if invoked. Its name is 'mit-' plus the current HEAD, and its
-- previous HEAD to undo to, plus an optional stash to apply after that.
recordUndoFile :: Text -> Maybe Text -> IO Text
recordUndoFile previousHead maybeStash = do
  Stdout head <- git ["rev-parse", "HEAD"]
  Text.writeFile (undofile head) contents
  pure head
  where
    contents :: Text
    contents =
      previousHead <> maybe "" (" " <>) maybeStash

syncMessage :: Text -> [Text] -> [Text] -> [Text]
syncMessage target commits conflicts =
  appendConflictsMessage commitsMessage ++ ["", undoMessage]
  where
    commitsMessage :: [Text]
    commitsMessage =
      "Synchronized with " <> Text.italic target <> "." :
      "" :
      map ("  " <>) commits
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

-- FIXME delete
syncFailedMessage :: Text -> [Text] -> [Text]
syncFailedMessage target conflicts =
  "Failed to synchronize with " <> Text.italic target <> "." :
  "" :
  map (("  " <>) . Text.red) conflicts
    ++ [ "",
         "If you would prefer not to resolve the conflicts at this time, please run",
         Text.bold (Text.blue "mit abort")
           <> ". Otherwise, please resolve the conflicts, then run "
           <> Text.bold (Text.blue "mit commit")
           <> "."
       ]

-- | Apply stash, return conflicts.
-- FIXME return action that returns conflicts instead, since they aren't always used
gitApplyStash :: Text -> IO [Text]
gitApplyStash stash = do
  git ["stash", "apply", "--quiet", stash] >>= \case
    ExitFailure _ -> do
      Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      () <- git ["reset", "--quiet"] -- unmerged (weird) -> unstaged (normal)
      pure conflicts
    ExitSuccess -> pure []

-- | Create a stash and blow away local changes (like 'git stash push')
gitCreateStash :: IO Text
gitCreateStash = do
  Stdout stash <- git ["stash", "create"]
  () <- git ["reset", "--hard", "--quiet", "HEAD"]
  pure stash

data DiffResult
  = Differences
  | NoDifferences

gitDiff :: IO DiffResult
gitDiff = do
  () <- git ["reset", "--quiet"]
  () <- git ["add", "--all", "--intent-to-add"]
  git ["diff", "--quiet"] <&> \case
    ExitFailure _ -> Differences
    ExitSuccess -> NoDifferences

-- | git fetch origin, at most once per run.
gitFetch :: IO ()
gitFetch = do
  fetched <- readIORef fetchedRef
  unless fetched do
    () <- git ["fetch", "--quiet", "origin"]
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
      Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      () <- git ["add", "--all"]
      -- TODO better commit message than the default
      () <- git ["commit", "--no-edit", "--quiet"]
      pure (MergeFailed conflicts)
    ExitSuccess -> pure MergeSucceeded

prettyCommitsBetween :: Text -> Text -> IO [Text]
prettyCommitsBetween commit1 commit2 =
  if commit1 == commit2
    then pure []
    else do
      Stdout commits <-
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
  Stdout v0 <- git ["--version"]
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

newtype Stdout a
  = Stdout a

newtype Stderr a
  = Stderr a

class ProcessOutput a where
  fromProcessOutput :: [Text] -> [Text] -> ExitCode -> IO a

instance ProcessOutput () where
  fromProcessOutput _ _ code =
    when (code /= ExitSuccess) (exitWith code)

instance ProcessOutput ExitCode where
  fromProcessOutput _ _ = pure

instance ProcessOutput (Stdout Text) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    case out of
      [] -> throwIO (userError "no stdout")
      line : _ -> pure (Stdout line)

instance a ~ Text => ProcessOutput (Stdout [a]) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    pure (Stdout out)

instance a ~ ExitCode => ProcessOutput (Either a (Stdout Text)) where
  fromProcessOutput out _ code =
    case code of
      ExitFailure _ -> pure (Left code)
      ExitSuccess ->
        case out of
          [] -> throwIO (userError "no stdout")
          line : _ -> pure (Right (Stdout line))

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
