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
    gitDeleteChanges,
    gitDiff,
    gitExistCommitsBetween,
    gitExistUntrackedFiles,
    gitFetch,
    gitFetch_,
    gitHead,
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
import Data.Text.IO qualified as Text
import GHC.Clock (getMonotonicTime)
import Ki qualified
import Mit.Env (Env (..))
import Mit.Monad
import Mit.Prelude
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Mit.Process
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals
import System.Process
import System.Process.Internals
import Text.Builder qualified
import Text.Builder.ANSI qualified as Text.Builder
import Text.Parsec qualified as Parsec

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
gitApplyStash :: Text -> Mit Env [GitConflict]
gitApplyStash stash = do
  conflicts <-
    git ["stash", "apply", "--quiet", stash] >>= \case
      False -> gitConflicts
      True -> pure []
  gitUnstageChanges
  pure conflicts

-- | Get the head of a local branch (refs/heads/...).
gitBranchHead :: Text -> Mit Env (Maybe Text)
gitBranchHead branch =
  git ["rev-parse", "refs/heads/" <> branch] <&> \case
    Left _ -> Nothing
    Right head -> Just head

-- | Get the directory a branch's worktree is checked out in, if it exists.
gitBranchWorktreeDir :: Text -> Mit Env (Maybe Text)
gitBranchWorktreeDir branch = do
  worktrees <- gitListWorktrees
  pure case List.find (\worktree -> worktree.branch == Just branch) worktrees of
    Nothing -> Nothing
    Just worktree -> Just worktree.directory

gitCommitsBetween :: Maybe Text -> Text -> Mit Env (Seq GitCommitInfo)
gitCommitsBetween commit1 commit2 =
  if commit1 == Just commit2
    then pure Seq.empty
    else do
      commits <-
        -- --first-parent seems desirable for topic branches
        git
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

gitConflicts :: Mit Env [GitConflict]
gitConflicts =
  mapMaybe parseGitConflict <$> git ["status", "--no-renames", "--porcelain=v1"]

-- | Get the conflicts with the given commitish.
--
-- Precondition: there is no merge in progress.
gitConflictsWith :: Text -> Mit Env [GitConflict]
gitConflictsWith commit = do
  maybeStash <- gitStash
  conflicts <- do
    git ["merge", "--no-commit", "--no-ff", commit] >>= \case
      False -> gitConflicts
      True -> pure []
  whenM gitMergeInProgress (git_ ["merge", "--abort"])
  whenJust maybeStash \stash -> git ["stash", "apply", "--quiet", stash]
  pure conflicts

gitCreateStash :: Mit Env (Maybe Text)
gitCreateStash = do
  git_ ["add", "--all"] -- it seems certain things (like renames), unless staged, cannot be stashed
  stash <- git ["stash", "create"]
  -- Even if the stash is Nothing, this still might be relevant/necessary.
  -- In particular, if there are only changes to submodule commits, we'll have staged them with 'git add' --all, then
  -- we'll have gotten no stash back from 'git stash create'.
  gitUnstageChanges
  pure stash

gitDefaultBranch :: Text -> Mit Env (Maybe Text)
gitDefaultBranch remote = do
  fmap (Text.drop (14 + Text.length remote))
    <$> git ["symbolic-ref", "refs/remotes/" <> remote <> "/HEAD"]

-- | Delete all changes in the index and working tree.
gitDeleteChanges :: Mit Env ()
gitDeleteChanges =
  git_ ["reset", "--hard", "--quiet", "HEAD"]

-- | Report whether there are any tracked, unstaged changes.
gitDiff :: Mit Env DiffResult
gitDiff = do
  git ["diff", "--quiet"] <&> \case
    False -> Differences
    True -> NoDifferences

gitExistCommitsBetween :: Text -> Text -> Mit Env Bool
gitExistCommitsBetween commit1 commit2 =
  if commit1 == commit2
    then pure False
    else isJust <$> git ["rev-list", "--max-count=1", commit1 <> ".." <> commit2]

-- | Do any untracked files exist?
gitExistUntrackedFiles :: Mit Env Bool
gitExistUntrackedFiles =
  not . null <$> gitListUntrackedFiles

gitFetch :: Text -> Mit Env Bool
gitFetch remote = do
  env <- getEnv
  if env.offline
    then pure False
    else do
      fetched <- io (readIORef fetchedRef)
      case Map.lookup remote fetched of
        Nothing -> do
          success <- git ["fetch", "--atomic", remote]
          io (writeIORef fetchedRef (Map.insert remote success fetched))
          pure success
        Just success -> pure success

-- Only fetch each remote at most once per run of `mit`
fetchedRef :: IORef (Map Text Bool)
fetchedRef =
  unsafePerformIO (newIORef mempty)
{-# NOINLINE fetchedRef #-}

gitFetch_ :: Text -> Mit Env ()
gitFetch_ =
  void . gitFetch

-- | Get the head commit.
gitHead :: Mit Env Text
gitHead =
  git ["rev-parse", "HEAD"]

-- | Get whether a commit is a merge commit.
gitIsMergeCommit :: Text -> Mit Env Bool
gitIsMergeCommit commit =
  git ["rev-parse", "--quiet", "--verify", commit <> "^2"]

gitLsFiles :: Mit Env [Text]
gitLsFiles =
  git ["ls-files"]

-- | List all untracked files.
gitListUntrackedFiles :: Mit Env [Text]
gitListUntrackedFiles =
  git ["ls-files", "--exclude-standard", "--other"]

-- | Get the head commit, if it exists.
gitMaybeHead :: Mit Env (Maybe Text)
gitMaybeHead =
  git ["rev-parse", "HEAD"] <&> \case
    Left _ -> Nothing
    Right commit -> Just commit

-- | Get whether a merge is in progress.
gitMergeInProgress :: Mit Env Bool
gitMergeInProgress = do
  gitdir <- gitRevParseAbsoluteGitDir
  io (doesFileExist (Text.unpack (gitdir <> "/MERGE_HEAD")))

-- | Does the given remote branch (refs/remotes/...) exist?
gitRemoteBranchExists :: Text -> Text -> Mit Env Bool
gitRemoteBranchExists remote branch =
  git ["rev-parse", "--quiet", "--verify", "refs/remotes/" <> remote <> "/" <> branch]

-- | Get the head of a remote branch.
gitRemoteBranchHead :: Text -> Text -> Mit Env (Maybe Text)
gitRemoteBranchHead remote branch =
  git ["rev-parse", "refs/remotes/" <> remote <> "/" <> branch] <&> \case
    Left _ -> Nothing
    Right head -> Just head

gitRevParseAbsoluteGitDir :: ProcessOutput a => Mit Env a
gitRevParseAbsoluteGitDir =
  git ["rev-parse", "--absolute-git-dir"]

gitShow :: Text -> Mit Env GitCommitInfo
gitShow commit =
  parseGitCommitInfo
    <$> git
      [ "show",
        "--color=always",
        "--date=human",
        "--format=format:%an\xFEFF%ad\xFEFF%H\xFEFF%h\xFEFF%s",
        commit
      ]

-- | Stash uncommitted changes (if any).
gitStash :: Mit Env (Maybe Text)
gitStash = do
  gitCreateStash >>= \case
    Nothing -> pure Nothing
    Just stash -> do
      git_ ["clean", "-d", "--force"]
      gitDeleteChanges
      pure (Just stash)

gitUnstageChanges :: Mit Env ()
gitUnstageChanges = do
  git_ ["reset", "--quiet", "--", "."]
  untrackedFiles <- gitListUntrackedFiles
  when (not (null untrackedFiles)) do
    git_ ("add" : "--intent-to-add" : untrackedFiles)

gitVersion :: Abort Pretty => Mit Env GitVersion
gitVersion = do
  v0 <- git ["--version"]
  fromMaybe (abort (Pretty.line ("Could not parse git version from: " <> Pretty.text v0))) do
    "git" : "version" : v1 : _ <- Just (Text.words v0)
    [sx, sy, sz] <- Just (Text.split (== '.') v1)
    x <- readMaybe (Text.unpack sx)
    y <- readMaybe (Text.unpack sy)
    z <- readMaybe (Text.unpack sz)
    pure (pure (GitVersion x y z))

data GitWorktree = GitWorktree
  { branch :: Maybe Text,
    commit :: Text,
    directory :: Text,
    prunable :: Bool
  }

-- | List worktrees.
gitListWorktrees :: Mit Env [GitWorktree]
gitListWorktrees = do
  git ["worktree", "list"] <&> map \line ->
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

git :: ProcessOutput a => [Text] -> Mit Env a
git args = do
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
  t0 <- io getMonotonicTime
  with (bracket (createProcess spec) cleanup) \(_maybeStdin, maybeStdout, maybeStderr, processHandle) -> do
    with Ki.scoped \scope -> do
      stdoutThread <- io (Ki.fork scope (drainTextHandle (fromJust maybeStdout)))
      stderrThread <- io (Ki.fork scope (drainTextHandle (fromJust maybeStderr)))
      exitCode <- io (waitForProcess processHandle)
      t1 <- io getMonotonicTime
      stdoutLines <- io (atomically (Ki.await stdoutThread))
      stderrLines <- io (atomically (Ki.await stderrThread))
      debugPrintGit args stdoutLines stderrLines exitCode (t1 - t0)
      io (fromProcessOutput stdoutLines stderrLines exitCode)
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

git_ :: [Text] -> Mit Env ()
git_ =
  git

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
--
-- FIXME bracket
git2 :: [Text] -> Mit Env Bool
git2 args = do
  t0 <- io getMonotonicTime
  (_, _, stderrHandle, processHandle) <-
    io do
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
    io do
      waitForProcess processHandle `catch` \case
        UserInterrupt -> pure (ExitFailure (-130))
        exception -> throwIO exception
  t1 <- io getMonotonicTime
  stderrLines <- io (drainTextHandle (fromJust stderrHandle))
  debugPrintGit args Seq.empty stderrLines exitCode (t1 - t0)
  pure case exitCode of
    ExitFailure _ -> False
    ExitSuccess -> True

debugPrintGit :: [Text] -> Seq Text -> Seq Text -> ExitCode -> Double -> Mit Env ()
debugPrintGit args stdoutLines stderrLines exitCode sec = do
  env <- getEnv
  io case env.verbosity of
    1 -> Pretty.put v1
    2 -> Pretty.put (v1 <> v2)
    _ -> pure ()
  where
    v1 =
      Pretty.line $
        Pretty.style (Text.Builder.bold . Text.Builder.brightBlack) $
          let prefix =
                marker
                  <> " ["
                  <> Pretty.builder (Text.Builder.fixedDouble 0 (sec * 1000))
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
