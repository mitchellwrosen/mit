-- | High-level git operations
module Mit.Git where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.ANSI qualified as Text.Builder
import Data.Text.IO qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Ki qualified
import Mit.Builder qualified as Builder
import Mit.Env (Env (..))
import Mit.GitCommand qualified as Git
import Mit.Monad
import Mit.Prelude
import Mit.Process
import Mit.Stanza
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals
import System.Posix.Terminal (queryTerminal)
import System.Process
import System.Process.Internals
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

prettyGitCommitInfo :: GitCommitInfo -> Text.Builder
prettyGitCommitInfo info =
  fold
    [ Text.Builder.bold (Text.Builder.black (Text.Builder.fromText info.shorthash)),
      Builder.space,
      Text.Builder.bold (Text.Builder.white (Text.Builder.fromText info.subject)),
      " - ",
      Text.Builder.italic (Text.Builder.white (Text.Builder.fromText info.author)),
      Builder.space,
      Text.Builder.italic (Text.Builder.yellow (Text.Builder.fromText info.date))
    ]

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

showGitConflict :: GitConflict -> Text.Builder
showGitConflict (GitConflict xy name) =
  Text.Builder.fromText name <> " (" <> showGitConflictXY xy <> ")"

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

showGitConflictXY :: GitConflictXY -> Text.Builder
showGitConflictXY = \case
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
gitApplyStash :: Text -> Mit Env x [GitConflict]
gitApplyStash stash = do
  conflicts <-
    Git.git (Git.StashApply Git.FlagQuiet stash) >>= \case
      False -> gitConflicts
      True -> pure []
  gitUnstageChanges
  pure conflicts

-- | Create a branch.
-- FIXME inline this
gitBranch :: Text -> Mit Env x ()
gitBranch branch =
  Git.git (Git.Branch Git.FlagNoTrack branch)

-- | Does the given local branch (refs/heads/...) exist?
gitBranchExists :: Text -> Mit Env x Bool
gitBranchExists branch =
  Git.git (Git.RevParse Git.FlagQuiet Git.FlagVerify ("refs/heads/" <> branch))

-- | Get the head of a local branch (refs/heads/...).
gitBranchHead :: Text -> Mit Env x (Maybe Text)
gitBranchHead branch =
  Git.git (Git.RevParse Git.NoFlagQuiet Git.NoFlagVerify ("refs/heads/" <> branch)) <&> \case
    Left _ -> Nothing
    Right head -> Just head

-- | Get the directory a branch's worktree is checked out in, if it exists.
gitBranchWorktreeDir :: Text -> Mit Env x (Maybe Text)
gitBranchWorktreeDir branch = do
  worktrees <- gitWorktreeList
  pure case List.find (\worktree -> worktree.branch == Just branch) worktrees of
    Nothing -> Nothing
    Just worktree -> Just worktree.directory

gitCommit :: Mit Env x Bool
gitCommit =
  io (queryTerminal 0) >>= \case
    False -> do
      message <- io (lookupEnv "MIT_COMMIT_MESSAGE")
      git ["commit", "--all", "--message", maybe "" Text.pack message]
    True ->
      git2 ["commit", "--patch", "--quiet"] <&> \case
        ExitFailure _ -> False
        ExitSuccess -> True

gitCommitsBetween :: Maybe Text -> Text -> Mit Env x (Seq GitCommitInfo)
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

gitConflicts :: Mit Env x [GitConflict]
gitConflicts =
  mapMaybe parseGitConflict <$> Git.git (Git.StatusV1 Git.FlagNoRenames)

-- | Get the conflicts with the given commitish.
--
-- Precondition: there is no merge in progress.
gitConflictsWith :: Text -> Mit Env x [GitConflict]
gitConflictsWith commit = do
  maybeStash <- gitStash
  conflicts <- do
    Git.git (Git.Merge Git.FlagNoCommit Git.FlagNoFF commit) >>= \case
      False -> gitConflicts
      True -> pure []
  whenM gitMergeInProgress (Git.git_ Git.MergeAbort)
  whenJust maybeStash \stash -> Git.git (Git.StashApply Git.FlagQuiet stash)
  pure conflicts

-- | Precondition: there are changes to stash
gitCreateStash :: Mit Env x Text
gitCreateStash = do
  Git.git_ Git.AddAll -- it seems certain things (like renames), unless staged, cannot be stashed
  stash <- Git.git Git.StashCreate
  gitUnstageChanges
  pure stash

gitDefaultBranch :: Text -> Mit Env x Text
gitDefaultBranch remote = do
  ref <- Git.git (Git.SymbolicRef ("refs/remotes/" <> remote <> "/HEAD"))
  pure (Text.drop (14 + Text.length remote) ref)

-- FIXME document this
gitDiff :: Mit Env x DiffResult
gitDiff = do
  gitUnstageChanges
  Git.git (Git.Diff Git.FlagQuiet) <&> \case
    False -> Differences
    True -> NoDifferences

gitExistCommitsBetween :: Text -> Text -> Mit Env x Bool
gitExistCommitsBetween commit1 commit2 =
  if commit1 == commit2
    then pure False
    else isJust <$> git ["rev-list", "--max-count=1", commit1 <> ".." <> commit2]

-- | Do any untracked files exist?
gitExistUntrackedFiles :: Mit Env x Bool
gitExistUntrackedFiles =
  not . null <$> gitListUntrackedFiles

gitFetch :: Text -> Mit Env x Bool
gitFetch remote = do
  fetched <- io (readIORef fetchedRef)
  case Map.lookup remote fetched of
    Nothing -> do
      success <- Git.git (Git.Fetch remote)
      io (writeIORef fetchedRef (Map.insert remote success fetched))
      pure success
    Just success -> pure success

-- Only fetch each remote at most once per run of `mit`
fetchedRef :: IORef (Map Text Bool)
fetchedRef =
  unsafePerformIO (newIORef mempty)
{-# NOINLINE fetchedRef #-}

gitFetch_ :: Text -> Mit Env x ()
gitFetch_ =
  void . gitFetch

gitHead :: Mit Env x Text
gitHead =
  Git.git (Git.RevParse Git.NoFlagQuiet Git.NoFlagVerify "HEAD")

gitIsMergeCommit :: Text -> Mit Env x Bool
gitIsMergeCommit commit =
  Git.git (Git.RevParse Git.FlagQuiet Git.FlagVerify (commit <> "^2"))

-- | List all untracked files.
gitListUntrackedFiles :: Mit Env x [Text]
gitListUntrackedFiles =
  git ["ls-files", "--exclude-standard", "--other"]

gitMergeInProgress :: Mit Env x Bool
gitMergeInProgress = do
  env <- getEnv
  io (doesFileExist (Text.unpack (env.gitdir <> "/MERGE_HEAD")))

gitPush :: Text -> Mit Env x Bool
gitPush branch =
  git ["push", "--set-upstream", "origin", "--quiet", branch <> ":" <> branch]

-- | Does the given remote branch (refs/remotes/...) exist?
gitRemoteBranchExists :: Text -> Text -> Mit Env x Bool
gitRemoteBranchExists remote branch =
  Git.git (Git.RevParse Git.FlagQuiet Git.FlagVerify ("refs/remotes/" <> remote <> "/" <> branch))

-- | Get the head of a remote branch.
gitRemoteBranchHead :: Text -> Text -> Mit Env x (Maybe Text)
gitRemoteBranchHead remote branch =
  Git.git (Git.RevParse Git.NoFlagQuiet Git.NoFlagVerify ("refs/remotes/" <> remote <> "/" <> branch)) <&> \case
    Left _ -> Nothing
    Right head -> Just head

gitRevParseAbsoluteGitDir :: Mit Env x (Maybe Text)
gitRevParseAbsoluteGitDir =
  git ["rev-parse", "--absolute-git-dir"] <&> \case
    Left _ -> Nothing
    Right dir -> Just dir

-- | The root of this git worktree.
gitRevParseShowToplevel :: Mit Env x Text
gitRevParseShowToplevel =
  git ["rev-parse", "--show-toplevel"]

gitShow :: Text -> Mit Env x GitCommitInfo
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
gitStash :: Mit Env x (Maybe Text)
gitStash = do
  gitDiff >>= \case
    Differences -> do
      stash <- gitCreateStash
      Git.git_ (Git.Clean Git.FlagD Git.FlagForce)
      Git.git_ (Git.Reset Git.Hard Git.FlagQuiet "HEAD")
      pure (Just stash)
    NoDifferences -> pure Nothing

gitUnstageChanges :: Mit Env x ()
gitUnstageChanges = do
  Git.git_ (Git.ResetPaths Git.FlagQuiet ["."])
  untrackedFiles <- gitListUntrackedFiles
  unless (null untrackedFiles) (Git.git_ (Git.Add Git.FlagIntentToAdd untrackedFiles))

gitVersion :: (forall void. [Stanza] -> Mit Env x void) -> Mit Env x GitVersion
gitVersion return = do
  v0 <- git ["--version"]
  fromMaybe (return [Just ("Could not parse git version from: " <> Text.Builder.fromText v0)]) do
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

-- /dir/one 0efd393c35 [oingo]         -> ("/dir/one", "0efd393c35", Just "oingo")
-- /dir/two dc0c114266 (detached HEAD) -> ("/dir/two", "dc0c114266", Nothing)
gitWorktreeList :: Mit Env x [GitWorktree]
gitWorktreeList = do
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

git :: ProcessOutput a => [Text] -> Mit Env x a
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
  with (bracket (createProcess spec) cleanup) \(_maybeStdin, maybeStdout, maybeStderr, processHandle) -> do
    with Ki.scoped \scope -> do
      stdoutThread <- io (Ki.fork scope (drainTextHandle (fromJust maybeStdout)))
      stderrThread <- io (Ki.fork scope (drainTextHandle (fromJust maybeStderr)))
      exitCode <- io (waitForProcess processHandle)
      stdoutLines <- io (atomically (Ki.await stdoutThread))
      stderrLines <- io (atomically (Ki.await stderrThread))
      debugPrintGit args stdoutLines stderrLines exitCode
      io (fromProcessOutput stdoutLines stderrLines exitCode)
  where
    cleanup :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
    cleanup (maybeStdin, maybeStdout, maybeStderr, process) =
      void @_ @ExitCode terminate `finally` closeHandles
      where
        closeHandles :: IO ()
        closeHandles =
          whenJust maybeStdin hClose
            `finally` whenJust maybeStdout hClose
            `finally` whenJust maybeStderr hClose
        terminate :: IO ExitCode
        terminate = do
          withProcessHandle process \case
            ClosedHandle _ -> pure ()
            OpenExtHandle {} -> bug "OpenExtHandle is Windows-only"
            OpenHandle pid -> do
              pgid <- getProcessGroupIDOf pid
              signalProcessGroup sigTERM pgid
          waitForProcess process

git_ :: [Text] -> Mit Env x ()
git_ =
  git

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
--
-- FIXME bracket
git2 :: [Text] -> Mit Env x ExitCode
git2 args = do
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
  stderrLines <- io (drainTextHandle (fromJust stderrHandle))
  debugPrintGit args Seq.empty stderrLines exitCode
  pure exitCode

debugPrintGit :: [Text] -> Seq Text -> Seq Text -> ExitCode -> Mit Env x ()
debugPrintGit args stdoutLines stderrLines exitCode = do
  env <- getEnv
  io case env.verbosity of
    1 -> Builder.putln (Text.Builder.brightBlack v1)
    2 -> Builder.putln (Text.Builder.brightBlack (v1 <> v2))
    _ -> pure ()
  where
    v1 = Text.Builder.bold (marker <> " git " <> Builder.hcat (map quote args))
    v2 = foldMap (\line -> "\n    " <> Text.Builder.fromText line) (stdoutLines <> stderrLines)

    quote :: Text -> Text.Builder
    quote s =
      if Text.any isSpace s
        then Builder.squoted (Text.Builder.fromText (Text.replace "'" "\\'" s))
        else Text.Builder.fromText s

    marker :: Text.Builder
    marker =
      case exitCode of
        ExitFailure _ -> Text.Builder.singleton '✗'
        ExitSuccess -> Text.Builder.singleton '✓'

drainTextHandle :: Handle -> IO (Seq Text)
drainTextHandle handle = do
  let loop acc =
        hIsEOF handle >>= \case
          False -> do
            line <- Text.hGetLine handle
            loop $! acc Seq.|> line
          True -> pure acc
  loop Seq.empty
