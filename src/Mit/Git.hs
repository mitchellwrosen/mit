-- | High-level git operations
module Mit.Git
  ( DiffResult (..),
    GitCommitInfo (..),
    prettyGitCommitInfo,
    GitConflict,
    prettyGitConflict,
    GitVersion (..),
    showGitVersion,
    git,
    git_,
    git2,
    gitApplyStash,
    gitBranchHead,
    gitBranchWorktreeDir,
    gitCommitsBetween,
    gitConflicts,
    gitConflictsWith,
    gitCreateStash,
    gitDiff,
    gitExistCommitsBetween,
    gitExistUntrackedFiles,
    gitFetch,
    gitFetch_,
    gitIsMergeCommit,
    gitLsFiles,
    gitMaybeHead,
    gitMergeInProgress,
    gitRemoteBranchExists,
    gitRemoteBranchHead,
    gitRevParseAbsoluteGitDir,
    gitUnstageChanges,
    gitVersion,
    -- unused, but useful? not sure
    gitDefaultBranch,
    gitShow,
    parseGitRepo,
  )
where

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as List1
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Text.Builder
import Data.Text.IO qualified as Text
import GHC.Clock (getMonotonicTime)
import Ki qualified
import Mit.Prelude
import Mit.Pretty qualified as Pretty
import Mit.Process
import Mit.Verbosity (Verbosity (V0, V1, V2))
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals
import System.Process
import System.Process.Internals
import Text.Builder.ANSI qualified as Text.Builder
import Text.Parsec qualified as Parsec
import Text.Printf (printf)

data DiffResult
  = Differences
  | NoDifferences

data GitCommitInfo = GitCommitInfo
  { author :: Text,
    date :: Text,
    hash :: Text,
    shorthash :: Text,
    subject :: Text
  }
  deriving stock (Show)

parseGitCommitInfo :: Text -> GitCommitInfo
parseGitCommitInfo line =
  case Text.split (== '\xFEFF') line of
    [author, date, hash, shorthash, subject] -> GitCommitInfo {author, date, hash, shorthash, subject}
    _ -> error (Text.unpack line)

prettyGitCommitInfo :: GitCommitInfo -> Pretty.Line
prettyGitCommitInfo info =
  Pretty.style (Text.Builder.bold . Text.Builder.black) (Pretty.text info.shorthash)
    <> Pretty.char ' '
    <> Pretty.style (Text.Builder.bold . Text.Builder.white) (Pretty.text info.subject)
    <> " - "
    <> Pretty.style (Text.Builder.italic . Text.Builder.white) (Pretty.text info.author)
    <> Pretty.char ' '
    <> Pretty.style (Text.Builder.italic . Text.Builder.yellow) (Pretty.text info.date)

-- FIXME some other color, magenta?

data GitConflict
  = GitConflict GitConflictXY Text
  deriving stock (Eq, Show)

-- FIXME
--
-- error: The following untracked working tree files would be overwritten by merge:
--         administration-client/administration-client.cabal
--         aeson-simspace/aeson-simspace.cabal
--         attack-designer/api/attack-designer-api.cabal
--         attack-designer/db/attack-designer-db.cabal
--         attack-designer/server/attack-designer-server.cabal
--         attack-integrations/attack-integrations.cabal
--         authz/simspace-authz.cabal
--         caching/caching.cabal
--         common-testlib/common-testlib.cabal
--         db-infra/db-infra.cabal
--         db-infra/migrations/0_migrate-rich-text-images-to-minio/migrate-rich-text-images-to-minio.cabal
--         db-infra/migrations/2.0.0.1010_migrate-questions-into-content-modules/range-data-server-migrate-questions-into-content-modules.cabal
--         db-infra/migrations/2.0.0.19_migrate-hello-table/range-data-server-migrate-hello-table.cabal
--         db-infra/migrations/2.0.0.21_migrate-puppet-yaml-to-text/range-data-server-migrate-puppet-yaml-to-text.cabal
--         db-infra/migrations/2.0.0.9015_migrate-refresh-stocks/range-data-server-migrate-refresh-stocks.cabal
--         db-infra/migrations/shared/range-data-server-migration.cabal
-- Please move or remove them before you merge.
-- Aborting
parseGitConflict :: Text -> Maybe GitConflict
parseGitConflict line = do
  [xy, name] <- Just (Text.words line)
  GitConflict <$> parseGitConflictXY xy <*> Just name

prettyGitConflict :: GitConflict -> Pretty.Line
prettyGitConflict (GitConflict xy name) =
  Pretty.text name <> " (" <> prettyGitConflictXY xy <> ")"

data GitConflictXY
  = AA -- both added
  | AU -- added by us
  | DD -- both deleted
  | DU -- deleted by us
  | UA -- added by them
  | UD -- deleted by them
  | UU -- both modified
  deriving stock (Eq, Show)

parseGitConflictXY :: Text -> Maybe GitConflictXY
parseGitConflictXY = \case
  "AA" -> Just AA
  "AU" -> Just AU
  "DD" -> Just DD
  "DU" -> Just DU
  "UA" -> Just UA
  "UD" -> Just UD
  "UU" -> Just UU
  _ -> Nothing

prettyGitConflictXY :: GitConflictXY -> Pretty.Line
prettyGitConflictXY = \case
  AA -> "both added"
  AU -> "added by us"
  DD -> "both deleted"
  DU -> "deleted by us"
  UA -> "added by them"
  UD -> "deleted by them"
  UU -> "both modified"

data GitVersion
  = GitVersion Int Int Int
  deriving stock (Eq, Ord)

showGitVersion :: GitVersion -> Text
showGitVersion (GitVersion x y z) =
  Text.pack (show x) <> "." <> Text.pack (show y) <> "." <> Text.pack (show z)

-- | Apply stash, return conflicts.
gitApplyStash :: Verbosity -> Text -> IO [GitConflict]
gitApplyStash verbosity stash = do
  conflicts <-
    git verbosity ["stash", "apply", "--quiet", stash] >>= \case
      False -> gitConflicts verbosity
      True -> pure []
  gitUnstageChanges verbosity
  pure conflicts

-- | Get the head of a local branch (refs/heads/...).
gitBranchHead :: Verbosity -> Text -> IO (Maybe Text)
gitBranchHead verbosity branch =
  git verbosity ["rev-parse", "refs/heads/" <> branch] <&> \case
    Left _ -> Nothing
    Right head -> Just head

-- | Get the directory a branch's worktree is checked out in, if it exists.
gitBranchWorktreeDir :: Verbosity -> Text -> IO (Maybe Text)
gitBranchWorktreeDir verbosity branch = do
  worktrees <- gitListWorktrees verbosity
  pure case List.find (\worktree -> worktree.branch == Just branch) worktrees of
    Nothing -> Nothing
    Just worktree -> Just worktree.directory

gitCommitsBetween :: Verbosity -> Maybe Text -> Text -> IO (Seq GitCommitInfo)
gitCommitsBetween verbosity commit1 commit2 =
  if commit1 == Just commit2
    then pure Seq.empty
    else do
      commits <-
        -- --first-parent seems desirable for topic branches
        git
          verbosity
          [ "rev-list",
            "--color=always",
            "--date=human",
            "--format=format:%an\xFEFF%ad\xFEFF%H\xFEFF%h\xFEFF%s",
            "--max-count=11",
            maybe id (\c1 c2 -> c1 <> ".." <> c2) commit1 commit2
          ]
      pure (parseGitCommitInfo <$> dropEvens commits)
  where
    -- git rev-list with a custom format prefixes every commit with a redundant line :|
    dropEvens :: Seq a -> Seq a
    dropEvens = \case
      _ Seq.:<| x Seq.:<| xs -> x Seq.<| dropEvens xs
      xs -> xs

gitConflicts :: Verbosity -> IO [GitConflict]
gitConflicts verbosity =
  mapMaybe parseGitConflict <$> git verbosity ["status", "--no-renames", "--porcelain=v1"]

-- | Get the conflicts with the given commitish.
--
-- Precondition: there is no merge in progress.
gitConflictsWith :: Verbosity -> Text -> IO [GitConflict]
gitConflictsWith verbosity commit = do
  maybeStash <- gitStash verbosity
  conflicts <- do
    git verbosity ["merge", "--no-commit", "--no-ff", commit] >>= \case
      False -> gitConflicts verbosity
      True -> pure []
  whenM (gitMergeInProgress verbosity) (git_ verbosity ["merge", "--abort"])
  whenJust maybeStash \stash -> git verbosity ["stash", "apply", "--quiet", stash]
  pure conflicts

gitCreateStash :: Verbosity -> IO (Maybe Text)
gitCreateStash verbosity = do
  git_ verbosity ["add", "--all"] -- it seems certain things (like renames), unless staged, cannot be stashed
  stash <- git verbosity ["stash", "create"]
  -- Even if the stash is Nothing, this still might be relevant/necessary.
  -- In particular, if there are only changes to submodule commits, we'll have staged them with 'git add' --all, then
  -- we'll have gotten no stash back from 'git stash create'.
  gitUnstageChanges verbosity
  pure stash

gitDefaultBranch :: Verbosity -> Text -> IO (Maybe Text)
gitDefaultBranch verbosity remote = do
  fmap (Text.drop (14 + Text.length remote))
    <$> git verbosity ["symbolic-ref", "refs/remotes/" <> remote <> "/HEAD"]

-- | Report whether there are any tracked, unstaged changes.
gitDiff :: Verbosity -> IO DiffResult
gitDiff verbosity = do
  git verbosity ["diff", "--quiet"] <&> \case
    False -> Differences
    True -> NoDifferences

gitExistCommitsBetween :: Verbosity -> Text -> Text -> IO Bool
gitExistCommitsBetween verbosity commit1 commit2 =
  if commit1 == commit2
    then pure False
    else isJust <$> git verbosity ["rev-list", "--max-count=1", commit1 <> ".." <> commit2]

-- | Do any untracked files exist?
gitExistUntrackedFiles :: Verbosity -> IO Bool
gitExistUntrackedFiles verbosity =
  not . null <$> gitListUntrackedFiles verbosity

gitFetch :: Verbosity -> Text -> IO Bool
gitFetch verbosity remote = do
  fetched <- readIORef fetchedRef
  case Map.lookup remote fetched of
    Nothing -> do
      success <- git verbosity ["fetch", "--atomic", remote]
      writeIORef fetchedRef (Map.insert remote success fetched)
      pure success
    Just success -> pure success

-- Only fetch each remote at most once per run of `mit`
fetchedRef :: IORef (Map Text Bool)
fetchedRef =
  unsafePerformIO (newIORef mempty)
{-# NOINLINE fetchedRef #-}

gitFetch_ :: Verbosity -> Text -> IO ()
gitFetch_ verbosity =
  void . gitFetch verbosity

-- | Get whether a commit is a merge commit.
gitIsMergeCommit :: Verbosity -> Text -> IO Bool
gitIsMergeCommit verbosity commit =
  git verbosity ["rev-parse", "--quiet", "--verify", commit <> "^2"]

gitLsFiles :: Verbosity -> IO [Text]
gitLsFiles verbosity =
  git verbosity ["ls-files"]

-- | List all untracked files.
gitListUntrackedFiles :: Verbosity -> IO [Text]
gitListUntrackedFiles verbosity =
  git verbosity ["ls-files", "--exclude-standard", "--other"]

-- | Get the head commit, if it exists.
gitMaybeHead :: Verbosity -> IO (Maybe Text)
gitMaybeHead verbosity =
  git verbosity ["rev-parse", "HEAD"] <&> \case
    Left _ -> Nothing
    Right commit -> Just commit

-- | Get whether a merge is in progress.
gitMergeInProgress :: Verbosity -> IO Bool
gitMergeInProgress verbosity = do
  gitdir <- gitRevParseAbsoluteGitDir verbosity
  doesFileExist (Text.unpack (gitdir <> "/MERGE_HEAD"))

-- | Does the given remote branch (refs/remotes/...) exist?
gitRemoteBranchExists :: Verbosity -> Text -> Text -> IO Bool
gitRemoteBranchExists verbosity remote branch =
  git verbosity ["rev-parse", "--quiet", "--verify", "refs/remotes/" <> remote <> "/" <> branch]

-- | Get the head of a remote branch.
gitRemoteBranchHead :: Verbosity -> Text -> Text -> IO (Maybe Text)
gitRemoteBranchHead verbosity remote branch =
  git verbosity ["rev-parse", "refs/remotes/" <> remote <> "/" <> branch] <&> \case
    Left _ -> Nothing
    Right head -> Just head

gitRevParseAbsoluteGitDir :: (ProcessOutput a) => Verbosity -> IO a
gitRevParseAbsoluteGitDir verbosity =
  git verbosity ["rev-parse", "--absolute-git-dir"]

gitShow :: Verbosity -> Text -> IO GitCommitInfo
gitShow verbosity commit =
  fmap parseGitCommitInfo do
    git
      verbosity
      [ "show",
        "--color=always",
        "--date=human",
        "--format=format:%an\xFEFF%ad\xFEFF%H\xFEFF%h\xFEFF%s",
        commit
      ]

-- | Stash uncommitted changes (if any).
gitStash :: Verbosity -> IO (Maybe Text)
gitStash verbosity = do
  gitCreateStash verbosity >>= \case
    Nothing -> pure Nothing
    Just stash -> do
      git_ verbosity ["clean", "-d", "--force"]
      git_ verbosity ["reset", "--hard", "--quiet", "HEAD"]
      pure (Just stash)

gitUnstageChanges :: Verbosity -> IO ()
gitUnstageChanges verbosity = do
  git_ verbosity ["reset", "--quiet", "--", "."]
  untrackedFiles <- gitListUntrackedFiles verbosity
  when (not (null untrackedFiles)) do
    git_ verbosity ("add" : "--intent-to-add" : untrackedFiles)

-- | Parse the @git@ version from the output of @git --version@.
--
-- If parsing fails, returns version @0.0.0@.
gitVersion :: Verbosity -> IO GitVersion
gitVersion verbosity = do
  v0 <- git verbosity ["--version"]
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
gitListWorktrees :: Verbosity -> IO [GitWorktree]
gitListWorktrees verbosity = do
  git verbosity ["worktree", "list"] <&> map \line ->
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
          Text.pack <$> Parsec.many1 (Parsec.satisfy (not . isSpace))

-- git@github.com:mitchellwrosen/mit.git -> Just ("git@github.com:mitchellwrosen/mit.git", "mit")
parseGitRepo :: Text -> Maybe (Text, Text)
parseGitRepo url = do
  url' <- Text.stripSuffix ".git" url
  pure (url, Text.takeWhileEnd (/= '/') url')

git :: (ProcessOutput a) => Verbosity -> [Text] -> IO a
git verbosity args = do
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
      stdoutLines <- atomically (Ki.await stdoutThread)
      stderrLines <- atomically (Ki.await stderrThread)
      debugPrintGit verbosity args stdoutLines stderrLines exitCode (t1 - t0)
      fromProcessOutput stdoutLines stderrLines exitCode
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

git_ :: Verbosity -> [Text] -> IO ()
git_ =
  git

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
--
-- FIXME bracket
git2 :: Verbosity -> [Text] -> IO Bool
git2 verbosity args = do
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
  stderrLines <- drainTextHandle (fromJust stderrHandle)
  debugPrintGit verbosity args Seq.empty stderrLines exitCode (t1 - t0)
  pure case exitCode of
    ExitFailure _ -> False
    ExitSuccess -> True

debugPrintGit :: Verbosity -> [Text] -> Seq Text -> Seq Text -> ExitCode -> Double -> IO ()
debugPrintGit verbosity args stdoutLines stderrLines exitCode sec = do
  case verbosity of
    V0 -> pure ()
    V1 -> Pretty.put v1
    V2 -> Pretty.put (v1 <> v2)
  where
    v1 =
      Pretty.line $
        Pretty.style (Text.Builder.bold . Text.Builder.brightBlack) $
          let prefix =
                marker
                  <> " ["
                  <> Pretty.builder (foldMap Text.Builder.fromChar (printf "%.0f" (sec * 1000) :: [Char]))
                  <> "ms] git "
           in case List1.nonEmpty args of
                -- fold (List.intersperse (Pretty.char ' ') (map quote args)) of
                Nothing -> prefix
                Just args1 -> prefix <> sconcat (List1.intersperse (Pretty.char ' ') (quote <$> args1))
    v2 =
      (stdoutLines <> stderrLines)
        & Foldable.toList
        & map (Pretty.style Text.Builder.brightBlack . Pretty.text)
        & Pretty.lines
        & Pretty.indent 4

    quote :: Text -> Pretty.Line
    quote s =
      if Text.any isSpace s
        then Pretty.char '\'' <> Pretty.text (Text.replace "'" "\\'" s) <> Pretty.char '\''
        else Pretty.text s

    marker :: Pretty.Line
    marker =
      case exitCode of
        ExitFailure _ -> Pretty.char '✗'
        ExitSuccess -> Pretty.char '✓'

drainTextHandle :: Handle -> IO (Seq Text)
drainTextHandle handle = do
  let loop acc =
        hIsEOF handle >>= \case
          False -> do
            line <- Text.hGetLine handle
            loop $! acc Seq.|> line
          True -> pure acc
  loop Seq.empty
