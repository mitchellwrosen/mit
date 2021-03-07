{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Main where

import Control.Category ((>>>))
import Control.Exception (AsyncException (UserInterrupt), IOException, catch, evaluate, throwIO, try)
import Control.Monad
import Data.Char
import Data.Foldable (fold, for_)
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ANSI qualified as Text
import Data.Text.Encoding.Base64 qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesDirectoryExist, doesFileExist, removeFile, withCurrentDirectory)
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
-- FIXME bail if active cherry-pick, active revert, active rebase, what else?
-- FIXME rev-list max 11, use ellipses after 10

-- TODO mit init
-- TODO mit delete-branch
-- TODO tweak things to work with git < 2.30.1
-- TODO rewrite mit commit algorithm in readme

main :: IO ()
main = do
  getArgs >>= \case
    ["branch", branch] -> mitBranch (Text.pack branch)
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> mitClone url name
    ["commit"] -> mitCommit
    ["merge", branch] -> mitMerge (Text.pack branch) -- temporary?
    ["sync"] -> mitSync
    ["undo"] -> mitUndo
    _ ->
      putLines
        [ "Usage:",
          "  mit branch ≪branch≫",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit merge ≪branch≫",
          "  mit sync",
          "  mit undo"
        ]

dieIfBuggyGit :: IO ()
dieIfBuggyGit = do
  version <- gitVersion
  case foldr (\(ver, err) acc -> if version < ver then (ver, err) : acc else acc) [] validations of
    [] -> pure ()
    errors ->
      die $
        map
          (\(ver, err) -> "Prior to " <> Text.bold "git" <> " version " <> showGitVersion ver <> ", " <> err)
          errors
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

dieIfMergeInProgress :: IO ()
dieIfMergeInProgress =
  whenM gitMergeInProgress (die [Text.bold "git merge" <> " in progress."])

dieIfNotInGitDir :: IO ()
dieIfNotInGitDir =
  try (evaluate gitdir) >>= \case
    Left (_ :: ExitCode) -> exitFailure
    Right _ -> pure ()

die :: [Text] -> IO a
die ss = do
  Text.putStr (Text.red (Text.unlines ss))
  exitFailure

mitBranch :: Text -> IO ()
mitBranch branch = do
  dieIfNotInGitDir

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

              _fetchResult :: Bool <-
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
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context <- makeContext
  case context.dirty of
    Differences -> mitCommitWith context
    NoDifferences -> mitSyncWith context -- n.b. not covered by tests (would be dupes of sync tests)

mitCommitWith :: Context -> IO ()
mitCommitWith context = do
  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> context.upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  remoteCommits :: [GitCommitInfo] <-
    case maybeUpstreamHead of
      Nothing -> pure []
      Just upstreamHead -> commitsBetween (Just context.head) upstreamHead

  localCommits0 :: [GitCommitInfo] <-
    commitsBetween maybeUpstreamHead "HEAD"

  -- FIXME actually make running 'mit commit' again work
  when (not (null remoteCommits) && null localCommits0) do
    putLines
      [ "",
        "  " <> Text.italic context.branch <> " is not up to date.",
        "",
        "  Run " <> Text.bold (Text.blue "mit sync") <> "first, or run " <> Text.bold (Text.blue "mit commit")
          <> " again to record a commit anyway.",
        ""
      ]
    exitFailure

  stash :: Text <-
    git ["stash", "create"]

  commitResult :: Bool <-
    gitCommit

  localCommits :: [GitCommitInfo] <-
    case commitResult of
      False -> pure localCommits0
      True -> commitsBetween maybeUpstreamHead "HEAD"

  pushResult :: Bool <-
    if not context.fetchFailed -- if fetch failed, assume we're offline
      && null remoteCommits -- if remote was ahead, we were already forked, or just forked
      && not (null localCommits) -- is there even anything to push?
      then gitPush context.branch
      else pure False

  canUndo :: Bool <-
    case pushResult of
      False -> do
        head <- git ["rev-parse", "HEAD"]
        case head == context.head of
          False -> do
            recordUndoFile context.branch64 [Reset context.head, Apply stash]
            pure True
          True -> pure False
      True ->
        case commitResult of
          False -> do
            deleteUndoFile context.branch64
            pure False
          True ->
            case localCommits of
              [_] -> do
                -- FIXME this call wouldn't be necessary if we don't pretty-print local commits right away
                head <- git ["rev-parse", "HEAD"]
                recordUndoFile context.branch64 [Revert head, Apply stash]
                pure True
              _ -> do
                deleteUndoFile context.branch64
                pure False

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        conflicts = [],
        syncs =
          [ Sync
              { commits = localCommits,
                source = context.branch,
                success = pushResult,
                target = context.upstream
              }
          ]
      }

mitMerge :: Text -> IO ()
mitMerge target0 = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context :: Context <-
    makeContext

  -- When given 'mit merge foo', prefer merging 'origin/foo' over 'foo'
  target :: Text <- do
    let remote = "origin/" <> target0
    git ["rev-parse", "--quiet", "--verify", remote] <&> \case
      False -> target0
      True -> remote

  maybeTargetCommit :: Maybe Text <-
    git ["rev-parse", target] <&> \case
      Left _ -> Nothing
      Right targetCommit -> Just targetCommit

  targetCommits :: [GitCommitInfo] <-
    case maybeTargetCommit of
      Nothing -> pure []
      Just targetCommit -> commitsBetween (Just context.head) targetCommit

  maybeStash :: Maybe Text <-
    case (targetCommits, context.dirty) of
      (_ : _, Differences) -> Just <$> gitStash
      _ -> pure Nothing

  mergeConflicts :: Maybe [Text] <-
    case targetCommits of
      [] -> pure Nothing
      _ ->
        fmap Just do
          gitMerge context.branch target >>= \case
            Left commitConflicts -> commitConflicts
            Right () -> pure []

  stashConflicts :: [Text] <-
    case maybeStash of
      Nothing -> pure []
      Just stash -> gitApplyStash stash

  canUndo :: Bool <- do
    head <- git ["rev-parse", "HEAD"]
    if head == context.head
      then pure False
      else do
        recordUndoFile context.branch64 (Reset context.head : maybeToList (Apply <$> maybeStash))
        pure True

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        conflicts = List.nub (stashConflicts ++ fromMaybe [] mergeConflicts),
        syncs =
          [ Sync
              { commits = targetCommits,
                source = target,
                success = maybe True null mergeConflicts,
                target = context.branch
              }
          ]
      }

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
-- FIXME don't push if any remote commits were applied locally
mitSync :: IO ()
mitSync = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context <- makeContext
  mitSyncWith context

mitSyncWith :: Context -> IO ()
mitSyncWith context = do
  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> context.upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  remoteCommits :: [GitCommitInfo] <-
    case maybeUpstreamHead of
      Nothing -> pure []
      Just upstreamHead -> commitsBetween (Just context.head) upstreamHead

  maybeStash :: Maybe Text <-
    case (remoteCommits, context.dirty) of
      (_ : _, Differences) -> Just <$> gitStash
      _ -> pure Nothing

  mergeConflicts :: Maybe [Text] <-
    case remoteCommits of
      [] -> pure Nothing
      _ ->
        fmap Just do
          gitMerge context.branch context.upstream >>= \case
            Left commitConflicts -> commitConflicts
            Right () -> pure []

  localCommits :: [GitCommitInfo] <-
    commitsBetween maybeUpstreamHead "HEAD"

  stashConflicts :: [Text] <-
    case maybeStash of
      Nothing -> pure []
      Just stash -> gitApplyStash stash

  pushResult :: Maybe Bool <-
    if not context.fetchFailed -- if fetch failed, assume we're offline
      && null remoteCommits -- don't push if we pulled anything, so it can be inspected locally
      && not (null localCommits) -- is there even anything to push?
      then Just <$> gitPush context.branch
      else pure Nothing

  canUndo :: Bool <-
    case pushResult of
      Just True -> do
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
        conflicts = List.nub (stashConflicts ++ fromMaybe [] mergeConflicts),
        syncs =
          [ Sync
              { commits = remoteCommits,
                source = context.upstream,
                success = maybe True null mergeConflicts,
                target = context.branch
              },
            Sync
              { commits = localCommits,
                source = context.branch,
                success = pushResult == Just True,
                target = context.upstream
              }
          ]
      }

-- FIXME output what we just undid
mitUndo :: IO ()
mitUndo = do
  dieIfNotInGitDir

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

data Context = Context
  { branch :: Text,
    branch64 :: Text,
    dirty :: DiffResult,
    fetchFailed :: Bool,
    head :: Text,
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

  pure
    Context
      { branch,
        branch64 = Text.encodeBase64 branch,
        dirty,
        fetchFailed,
        head,
        upstream
      }

data Summary = Summary
  { branch :: Text,
    canUndo :: Bool,
    conflicts :: [Text],
    syncs :: [Sync]
  }

-- FIXME add "yellow" success for could-have-pushed-but-didnt?
data Sync = Sync
  { commits :: [GitCommitInfo],
    source :: Text,
    success :: Bool,
    target :: Text
  }

-- FIXME show some graph of where local/remote is at
putSummary :: Summary -> IO ()
putSummary summary =
  let output = concatMap syncLines summary.syncs ++ conflictsLines ++ undoLines
   in if null output then pure () else putLines ("" : output)
  where
    conflictsLines :: [Text]
    conflictsLines =
      if null summary.conflicts
        then []
        else "  The following files have conflicts." : map (("    " <>) . Text.red) summary.conflicts ++ [""]
    syncLines :: Sync -> [Text]
    syncLines sync =
      if null sync.commits
        then []
        else
          colorize (Text.italic ("  " <> sync.source <> " → " <> sync.target)) :
          map (("  " <>) . prettyGitCommitInfo) sync.commits
            ++ [""]
      where
        colorize :: Text -> Text
        colorize =
          if sync.success then Text.green else Text.red
    undoLines :: [Text]
    undoLines =
      if summary.canUndo
        then ["  Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change.", ""]
        else []

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

-- | Do any untracked files exist?
gitExistUntrackedFiles :: IO Bool
gitExistUntrackedFiles = do
  files :: [Text] <- git ["ls-files", "--exclude-standard", "--other"]
  pure (not (null files))

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
gitMerge :: Text -> Text -> IO (Either (IO [Text]) ())
gitMerge me target = do
  git ["merge", "--ff", "--no-commit", "--quiet", target] >>= \case
    False ->
      (pure . Left) do
        conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
        git_ ["add", "--all"]
        git_ ["commit", "--no-edit", "--quiet", "--message", mergeMessage conflicts]
        pure conflicts
    True -> do
      whenM gitMergeInProgress (git_ ["commit", "--message", mergeMessage []])
      pure (Right ())
  where
    mergeMessage :: [Text] -> Text
    mergeMessage conflicts =
      -- FIXME use builder
      fold
        [ "⅄",
          if null conflicts then "" else "\x0338",
          " ",
          if target' == me then me else target' <> " → " <> me,
          if null conflicts
            then ""
            else " (conflicts)\n\nConflicting files:\n" <> Text.intercalate "\n" (map ("  " <>) conflicts)
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
  git ["push", "--quiet", "--set-upstream", "origin", branch <> ":" <> branch]

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
      Text.italic (Text.yellow info.date)
    ]

commitsBetween :: Maybe Text -> Text -> IO [GitCommitInfo]
commitsBetween commit1 commit2 =
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
