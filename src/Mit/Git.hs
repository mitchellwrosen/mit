-- | High-level git operations
module Mit.Git
  ( DiffResult (..),
    GitCommitInfo (..),
    GitConflict (..),
    GitConflictXY (..),
    GitVersion (..),
    showGitVersion,
    git,
    git2,
    gitApplyStash,
    gitBranchWorktreeDir,
    gitCommitsBetween,
    gitConflicts,
    gitCreateStash,
    gitCurrentBranch,
    gitDiff,
    gitExistCommitsBetween,
    gitExistUntrackedFiles,
    gitFetch,
    gitIsMergeCommit,
    gitMaybeHead,
    gitMergeInProgress,
    gitNumCommitsBetween,
    gitNumCommitsOn,
    gitRemoteBranchExists,
    gitRemoteBranchHead,
    gitRevParseAbsoluteGitDir,
    gitUnstageChanges,
    gitVersion,
    -- unused, but useful? not sure
    gitConflictsWith,
    gitDefaultBranch,
    gitShow,
    parseGitRepo,
  )
where

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Read qualified as Text.Read
import GHC.Clock (getMonotonicTime)
import Ki qualified
import Mit.Git.GitCommitInfo (GitCommitInfo (..), parseGitCommitInfo)
import Mit.Git.GitConflict (GitConflict (..), GitConflictXY (..), parseGitConflict)
import Mit.Logger (Logger, log)
import Mit.Output (ProcessInfo1 (..))
import Mit.Prelude
import Mit.Process (ProcessOutput (..))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals (sigTERM, signalProcessGroup)
import System.Process
  ( CmdSpec (RawCommand),
    CreateProcess (..),
    ProcessHandle,
    StdStream (CreatePipe, Inherit, NoStream),
    createProcess,
    waitForProcess,
  )
import System.Process.Internals (ProcessHandle__ (ClosedHandle, OpenExtHandle, OpenHandle), withProcessHandle)
import Text.Parsec qualified as Parsec

data DiffResult
  = Differences
  | NoDifferences

data GitVersion
  = GitVersion Int Int Int
  deriving stock (Eq, Ord)

showGitVersion :: GitVersion -> Text
showGitVersion (GitVersion x y z) =
  Text.pack (show x) <> "." <> Text.pack (show y) <> "." <> Text.pack (show z)

-- | Apply stash, return conflicts.
gitApplyStash :: Logger ProcessInfo1 -> Text -> IO [GitConflict]
gitApplyStash logger stash = do
  conflicts <-
    git logger ["stash", "apply", "--quiet", stash] >>= \case
      False -> gitConflicts logger
      True -> pure []
  gitUnstageChanges logger
  pure conflicts

-- | Get the directory a branch's worktree is checked out in, if it exists.
gitBranchWorktreeDir :: Logger ProcessInfo1 -> Text -> IO (Maybe Text)
gitBranchWorktreeDir logger branch = do
  worktrees <- gitListWorktrees logger
  pure case List.find (\worktree -> worktree.branch == Just branch) worktrees of
    Nothing -> Nothing
    Just worktree -> Just worktree.directory

gitCommitsBetween :: Logger ProcessInfo1 -> Maybe Text -> Text -> IO (Seq GitCommitInfo)
gitCommitsBetween logger commit1 commit2 =
  if commit1 == Just commit2
    then pure Seq.empty
    else do
      commits <-
        -- --first-parent seems desirable for topic branches
        git
          logger
          [ "rev-list",
            "--format=format:%an\xFEFF%ah\xFEFF%H\xFEFF%h\xFEFF%s",
            maybe id (\c1 c2 -> c1 <> ".." <> c2) commit1 commit2
          ]
      pure (parseGitCommitInfo <$> dropEvens commits)
  where
    -- git rev-list with a custom format prefixes every commit with a redundant line :|
    dropEvens :: Seq a -> Seq a
    dropEvens = \case
      _ Seq.:<| x Seq.:<| xs -> x Seq.<| dropEvens xs
      xs -> xs

gitConflicts :: Logger ProcessInfo1 -> IO [GitConflict]
gitConflicts logger =
  mapMaybe parseGitConflict <$> git logger ["status", "--no-renames", "--porcelain=v1"]

-- | Get the conflicts with the given commitish.
--
-- Precondition: there is no merge in progress.
gitConflictsWith :: Logger ProcessInfo1 -> Text -> Text -> IO [GitConflict]
gitConflictsWith logger gitdir commit = do
  maybeStash <- gitStash logger
  conflicts <- do
    git logger ["merge", "--no-commit", "--no-ff", commit] >>= \case
      False -> gitConflicts logger
      True -> pure []
  whenM (gitMergeInProgress gitdir) (git @() logger ["merge", "--abort"])
  whenJust maybeStash \stash -> git logger ["stash", "apply", "--quiet", stash]
  pure conflicts

gitCreateStash :: Logger ProcessInfo1 -> IO (Maybe Text)
gitCreateStash logger = do
  git @() logger ["add", "--all"] -- it seems certain things (like renames), unless staged, cannot be stashed
  stash <-
    git logger ["stash", "create"] <&> \case
      Seq.Empty -> Nothing
      stash Seq.:<| _ -> Just stash
  -- Even if the stash is Nothing, this still might be relevant/necessary.
  -- In particular, if there are only changes to submodule commits, we'll have staged them with 'git add' --all, then
  -- we'll have gotten no stash back from 'git stash create'.
  gitUnstageChanges logger
  pure stash

-- | Get the name of the current branch.
gitCurrentBranch :: Logger ProcessInfo1 -> IO (Maybe Text)
gitCurrentBranch logger =
  git logger ["branch", "--show-current"] <&> \case
    Seq.Empty -> Nothing
    branch Seq.:<| _ -> Just branch

gitDefaultBranch :: Logger ProcessInfo1 -> Text -> IO (Maybe Text)
gitDefaultBranch logger remote =
  git logger ["symbolic-ref", "refs/remotes/" <> remote <> "/HEAD"] <&> \case
    Left _code -> Nothing
    Right branch -> Just (Text.drop (14 + Text.length remote) branch)

-- | Report whether there are any tracked, unstaged changes.
gitDiff :: Logger ProcessInfo1 -> IO DiffResult
gitDiff logger = do
  git logger ["diff", "--quiet"] <&> \case
    False -> Differences
    True -> NoDifferences

gitExistCommitsBetween :: Logger ProcessInfo1 -> Text -> Text -> IO Bool
gitExistCommitsBetween logger commit1 commit2 =
  if commit1 == commit2
    then pure False
    else do
      commits <- git logger ["rev-list", "--max-count=1", commit1 <> ".." <> commit2]
      pure (not (Seq.null commits))

-- | Do any untracked files exist?
gitExistUntrackedFiles :: Logger ProcessInfo1 -> IO Bool
gitExistUntrackedFiles logger =
  not . null <$> gitListUntrackedFiles logger

gitFetch :: Logger ProcessInfo1 -> Text -> IO Bool
gitFetch logger remote = do
  fetched <- readIORef fetchedRef
  case Map.lookup remote fetched of
    Nothing -> do
      success <- git logger ["fetch", "--atomic", remote]
      writeIORef fetchedRef (Map.insert remote success fetched)
      pure success
    Just success -> pure success

-- Only fetch each remote at most once per run of `mit`
fetchedRef :: IORef (Map Text Bool)
fetchedRef =
  unsafePerformIO (newIORef mempty)
{-# NOINLINE fetchedRef #-}

-- | Get whether a commit is a merge commit.
gitIsMergeCommit :: Logger ProcessInfo1 -> Text -> IO Bool
gitIsMergeCommit logger commit =
  git logger ["rev-parse", "--quiet", "--verify", commit <> "^2"]

-- | List all untracked files.
gitListUntrackedFiles :: Logger ProcessInfo1 -> IO [Text]
gitListUntrackedFiles logger =
  git logger ["ls-files", "--exclude-standard", "--other"]

-- | Get the head commit, if it exists.
gitMaybeHead :: Logger ProcessInfo1 -> IO (Maybe Text)
gitMaybeHead logger =
  git logger ["rev-parse", "HEAD"] <&> \case
    Left _ -> Nothing
    Right commit -> Just commit

-- | Get whether a merge is in progress.
gitMergeInProgress :: Text -> IO Bool
gitMergeInProgress gitdir =
  doesFileExist (Text.unpack (gitdir <> "/MERGE_HEAD"))

gitNumCommitsBetween :: Logger ProcessInfo1 -> Text -> Text -> IO Int
gitNumCommitsBetween logger commit1 commit2 =
  if commit1 == commit2
    then pure 0
    else do
      commitsString <- git logger ["rev-list", "--count", commit1 <> ".." <> commit2]
      pure case Text.Read.decimal commitsString of
        Right (commits, "") -> commits
        _ -> 0

gitNumCommitsOn :: Logger ProcessInfo1 -> Text -> IO Int
gitNumCommitsOn logger commit = do
  commitsString <- git logger ["rev-list", "--count", commit]
  pure case Text.Read.decimal commitsString of
    Right (commits, "") -> commits
    _ -> 0

-- | Does the given remote branch (refs/remotes/...) exist?
gitRemoteBranchExists :: Logger ProcessInfo1 -> Text -> Text -> IO Bool
gitRemoteBranchExists logger remote branch =
  git logger ["rev-parse", "--quiet", "--verify", "refs/remotes/" <> remote <> "/" <> branch]

-- | Get the head of a remote branch.
gitRemoteBranchHead :: Logger ProcessInfo1 -> Text -> Text -> IO (Maybe Text)
gitRemoteBranchHead logger remote branch =
  git logger ["rev-parse", "refs/remotes/" <> remote <> "/" <> branch] <&> \case
    Left _ -> Nothing
    Right head -> Just head

gitRevParseAbsoluteGitDir :: Logger ProcessInfo1 -> IO (Maybe Text)
gitRevParseAbsoluteGitDir logger =
  git logger ["rev-parse", "--absolute-git-dir"] <&> \case
    Left _ -> Nothing
    Right gitdir -> Just gitdir

gitShow :: Logger ProcessInfo1 -> Text -> IO GitCommitInfo
gitShow logger commit =
  fmap parseGitCommitInfo do
    git
      logger
      [ "show",
        "--color=always",
        "--date=human",
        "--format=format:%an\xFEFF%ad\xFEFF%H\xFEFF%h\xFEFF%s",
        commit
      ]

-- | Stash uncommitted changes (if any).
gitStash :: Logger ProcessInfo1 -> IO (Maybe Text)
gitStash logger = do
  gitCreateStash logger >>= \case
    Nothing -> pure Nothing
    Just stash -> do
      git @() logger ["clean", "-d", "--force"]
      git @() logger ["reset", "--hard", "--quiet", "HEAD"]
      pure (Just stash)

gitUnstageChanges :: Logger ProcessInfo1 -> IO ()
gitUnstageChanges logger = do
  git @() logger ["reset", "--quiet", "--", "."]
  untrackedFiles <- gitListUntrackedFiles logger
  when (not (null untrackedFiles)) do
    git @() logger ("add" : "--intent-to-add" : untrackedFiles)

-- | Parse the @git@ version from the output of @git --version@.
--
-- If parsing fails, returns version @0.0.0@.
gitVersion :: Logger ProcessInfo1 -> IO GitVersion
gitVersion logger = do
  v0 <- git logger ["--version"]
  pure do
    fromMaybe (GitVersion 0 0 0) do
      "git" : "version" : v1 : _ <- Just (Text.words v0)
      [sx, sy, sz] <- Just (Text.split (== '.') v1)
      x <- readMaybe (Text.unpack sx)
      y <- readMaybe (Text.unpack sy)
      z <- readMaybe (Text.unpack sz)
      pure (GitVersion x y z)

data GitWorktree = GitWorktree
  { branch :: Maybe Text,
    commit :: Text,
    directory :: Text,
    prunable :: Bool
  }

-- | List worktrees.
gitListWorktrees :: Logger ProcessInfo1 -> IO [GitWorktree]
gitListWorktrees logger = do
  git logger ["worktree", "list"] <&> map \line ->
    case Parsec.parse parser "" line of
      Left err -> error (show err)
      Right worktree -> worktree
  where
    parser :: Parsec.Parsec Text () GitWorktree
    parser = do
      directory <- segmentP
      Parsec.spaces
      commit <- segmentP
      Parsec.spaces
      branch <-
        asum
          [ Nothing <$ Parsec.string "(detached HEAD)",
            fmap Just do
              _ <- Parsec.char '['
              branch <- Parsec.manyTill Parsec.anyChar (Parsec.char ']')
              pure (Text.pack branch)
          ]
      Parsec.spaces
      prunable <-
        asum
          [ True <$ Parsec.string "prunable",
            pure False
          ]
      pure GitWorktree {branch, commit, directory, prunable}
      where
        segmentP :: Parsec.Parsec Text () Text
        segmentP =
          Text.pack <$> Parsec.many1 (Parsec.satisfy (not . Char.isSpace))

-- git@github.com:mitchellwrosen/mit.git -> Just ("git@github.com:mitchellwrosen/mit.git", "mit")
parseGitRepo :: Text -> Maybe (Text, Text)
parseGitRepo url = do
  url' <- Text.stripSuffix ".git" url
  pure (url, Text.takeWhileEnd (/= '/') url')

git :: (ProcessOutput a) => Logger ProcessInfo1 -> [Text] -> IO a
git logger args = do
  let spec :: CreateProcess
      spec =
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
  t0 <- getMonotonicTime
  bracket (createProcess spec) cleanup \(_maybeStdin, maybeStdout, maybeStderr, processHandle) -> do
    Ki.scoped \scope -> do
      stdoutThread <- Ki.fork scope (drainTextHandle (fromJust maybeStdout))
      stderrThread <- Ki.fork scope (drainTextHandle (fromJust maybeStderr))
      exitCode <- waitForProcess processHandle
      t1 <- getMonotonicTime
      output <- atomically (Ki.await stdoutThread)
      errput <- atomically (Ki.await stderrThread)
      log
        logger
        ProcessInfo1
          { name = "git",
            args,
            output,
            errput,
            exitCode,
            seconds = t1 - t0
          }
      fromProcessOutput output errput exitCode
  where
    cleanup :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
    cleanup (maybeStdin, maybeStdout, maybeStderr, process) =
      ignoreSyncExceptions (terminate `finally` closeHandles)
      where
        closeHandles :: IO ()
        closeHandles =
          whenJust maybeStdin hClose
            `finally` whenJust maybeStdout hClose
            `finally` whenJust maybeStderr hClose
        terminate :: IO ()
        terminate = do
          withProcessHandle process \case
            ClosedHandle _ -> pure ()
            OpenExtHandle {} -> bug "OpenExtHandle is Windows-only"
            OpenHandle pid -> do
              pgid <- getProcessGroupIDOf pid
              signalProcessGroup sigTERM pgid
          _exitCode <- waitForProcess process
          pure ()

ignoreSyncExceptions :: IO () -> IO ()
ignoreSyncExceptions action =
  try action >>= \case
    Left ex ->
      case fromException ex of
        Nothing -> pure ()
        Just (_ :: SomeAsyncException) -> throwIO ex
    Right () -> pure ()

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
--
-- FIXME bracket
git2 :: Logger ProcessInfo1 -> [Text] -> IO Bool
git2 logger args = do
  t0 <- getMonotonicTime
  (_, _, stderrHandle, processHandle) <- do
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
  exitCode <- do
    waitForProcess processHandle `catch` \case
      UserInterrupt -> pure (ExitFailure (-130))
      exception -> throwIO exception
  t1 <- getMonotonicTime
  errput <- drainTextHandle (fromJust stderrHandle)
  log
    logger
    ProcessInfo1
      { name = "git",
        args,
        output = Seq.empty,
        errput,
        exitCode,
        seconds = t1 - t0
      }
  pure case exitCode of
    ExitFailure _ -> False
    ExitSuccess -> True

drainTextHandle :: Handle -> IO (Seq Text)
drainTextHandle handle = do
  let loop acc =
        hIsEOF handle >>= \case
          False -> do
            line <- Text.hGetLine handle
            loop $! acc Seq.|> line
          True -> pure acc
  loop Seq.empty
