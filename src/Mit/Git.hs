{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Mit.Git where

import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import Mit.Globals (debug)
import Mit.Prelude
import Mit.Process
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Terminal (queryTerminal)
import System.Process

-- FIXME this finds the wrong dir for worktrees
gitdir :: Text
gitdir =
  unsafePerformIO (git ["rev-parse", "--absolute-git-dir"])
{-# NOINLINE gitdir #-}

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

prettyGitCommitInfo :: GitCommitInfo -> Text
prettyGitCommitInfo info =
  -- FIXME use builder
  fold
    [ Text.bold (Text.black info.shorthash),
      " ",
      Text.bold (Text.white info.subject),
      " - ",
      Text.italic (Text.white info.author),
      " ",
      Text.italic (Text.yellow info.date) -- FIXME some other color, magenta?
    ]

data GitConflict
  = GitConflict GitConflictXY Text
  deriving stock (Eq, Show)

parseGitConflict :: Text -> Maybe GitConflict
parseGitConflict line = do
  [xy, name] <- Just (Text.words line)
  GitConflict <$> parseGitConflictXY xy <*> Just name

-- FIXME builder
showGitConflict :: GitConflict -> Text
showGitConflict (GitConflict xy name) =
  name <> " (" <> showGitConflictXY xy <> ")"

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

-- FIXME builder
showGitConflictXY :: GitConflictXY -> Text
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
gitApplyStash :: Text -> IO [GitConflict]
gitApplyStash stash = do
  conflicts <-
    git ["stash", "apply", stash] >>= \case
      False -> gitConflicts
      True -> pure []
  gitUnstageChanges
  pure conflicts

gitCommit :: IO Bool
gitCommit =
  queryTerminal 0 >>= \case
    False -> do
      message <- lookupEnv "MIT_COMMIT_MESSAGE"
      git ["commit", "--all", "--message", maybe "" Text.pack message]
    True ->
      git2 ["commit", "--patch", "--quiet"] <&> \case
        ExitFailure _ -> False
        ExitSuccess -> True

gitCommitsBetween :: Maybe Text -> Text -> IO [GitCommitInfo]
gitCommitsBetween commit1 commit2 =
  if commit1 == Just commit2
    then pure []
    else do
      commits <-
        -- --first-parent seems desirable for topic branches
        git
          [ "rev-list",
            "--color=always",
            "--date=human",
            "--format=format:%an\xFEFF%ad\xFEFF%H\xFEFF%h\xFEFF%s",
            "--max-count=10",
            maybe id (\c1 c2 -> c1 <> ".." <> c2) commit1 commit2
          ]
      pure (map parseCommitInfo (dropEvens commits))
  where
    -- git rev-list with a custom format prefixes every commit with a redundant line :|
    dropEvens :: [a] -> [a]
    dropEvens = \case
      _ : x : xs -> x : dropEvens xs
      xs -> xs
    parseCommitInfo :: Text -> GitCommitInfo
    parseCommitInfo line =
      case Text.split (== '\xFEFF') line of
        [author, date, hash, shorthash, subject] -> GitCommitInfo {author, date, hash, shorthash, subject}
        _ -> error (Text.unpack line)

gitCreateStash :: IO Text
gitCreateStash = do
  git_ ["add", "--all"] -- it seems certain things (like renames), unless staged, cannot be stashed
  stash <- git ["stash", "create"]
  gitUnstageChanges
  pure stash

-- | Get the current branch.
gitCurrentBranch :: IO Text
gitCurrentBranch =
  git ["branch", "--show-current"]

-- FIXME document this
gitDiff :: IO DiffResult
gitDiff = do
  gitUnstageChanges
  git ["diff", "--quiet"] <&> \case
    False -> Differences
    True -> NoDifferences

-- | Do any untracked files exist?
gitExistUntrackedFiles :: IO Bool
gitExistUntrackedFiles =
  not . null <$> gitListUntrackedFiles

gitConflicts :: IO [GitConflict]
gitConflicts =
  mapMaybe parseGitConflict <$> git ["status", "--no-renames", "--porcelain=v1"]

-- | List all untracked files.
gitListUntrackedFiles :: IO [Text]
gitListUntrackedFiles =
  git ["ls-files", "--exclude-standard", "--other"]

-- FIXME document what this does
gitMerge :: Text -> Text -> IO (Either (IO [GitConflict]) ())
gitMerge me target = do
  git ["merge", "--ff", "--no-commit", target] >>= \case
    False ->
      (pure . Left) do
        conflicts <- gitConflicts
        git_ ["add", "--all"]
        git_ ["commit", "--no-edit", "--message", mergeMessage conflicts]
        pure conflicts
    True -> do
      whenM gitMergeInProgress (git_ ["commit", "--message", mergeMessage []])
      pure (Right ())
  where
    mergeMessage :: [GitConflict] -> Text
    mergeMessage conflicts =
      -- FIXME use builder
      fold
        [ "⅄",
          if null conflicts then "" else "\x0338",
          " ",
          if target' == me then me else target' <> " → " <> me,
          if null conflicts
            then ""
            else
              " (conflicts)\n\nConflicting files:\n"
                <> Text.intercalate "\n" (map (("  " <>) . showGitConflict) conflicts)
        ]
      where
        target' :: Text
        target' =
          fromMaybe target (Text.stripPrefix "origin/" target)

gitMergeInProgress :: IO Bool
gitMergeInProgress =
  doesFileExist (Text.unpack (gitdir <> "/MERGE_HEAD"))

gitPush :: Text -> IO Bool
gitPush branch =
  git ["push", "--set-upstream", "origin", branch <> ":" <> branch]

-- | Blow away untracked files, and hard-reset to the given commit
gitResetHard :: Text -> IO ()
gitResetHard commit = do
  git_ ["clean", "-d", "--force"]
  git ["reset", "--hard", commit]

-- | Stash uncommitted changes (if any).
gitStash :: IO (Maybe Text)
gitStash = do
  gitDiff >>= \case
    Differences -> do
      stash <- gitCreateStash
      gitResetHard "HEAD"
      pure (Just stash)
    NoDifferences -> pure Nothing

gitUnstageChanges :: IO ()
gitUnstageChanges = do
  git_ ["reset", "--mixed"]
  untrackedFiles <- gitListUntrackedFiles
  unless (null untrackedFiles) (git_ ("add" : "--intent-to-add" : untrackedFiles))

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

-- git@github.com:mitchellwrosen/mit.git -> Just ("git@github.com:mitchellwrosen/mit.git", "mit")
parseGitRepo :: Text -> Maybe (Text, Text)
parseGitRepo url = do
  url' <- Text.stripSuffix ".git" url
  pure (url, Text.takeWhileEnd (/= '/') url')

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
  debugPrintGit args stdoutLines stderrLines exitCode
  fromProcessOutput stdoutLines stderrLines exitCode

git_ :: [Text] -> IO ()
git_ =
  git

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
git2 :: [Text] -> IO ExitCode
git2 args = do
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
  debugPrintGit args [] stderrLines exitCode
  pure exitCode

debugPrintGit :: [Text] -> [Text] -> [Text] -> ExitCode -> IO ()
debugPrintGit args stdoutLines stderrLines exitCode =
  when debug do
    putLines do
      let output :: [Text]
          output =
            map (Text.brightBlack . ("    " <>)) (stdoutLines ++ stderrLines)
      Text.bold (Text.brightBlack (Text.unwords (marker <> " git" : map quoteText args))) : output
  where
    marker :: Text
    marker =
      case exitCode of
        ExitFailure _ -> "✗"
        ExitSuccess -> "✓"
