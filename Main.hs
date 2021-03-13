{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Main where

import Control.Category ((>>>))
import Control.Exception (AsyncException (UserInterrupt), IOException, catch, evaluate, throwIO, try)
import Control.Monad
import Data.Char
import Data.Foldable (fold, for_)
import Data.Function
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import qualified System.Clock as Clock
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
-- FIXME test file deleted by us/them conflict

-- TODO mit init
-- TODO mit delete-branch
-- TODO tweak things to work with git < 2.30.1
-- TODO rewrite mit commit algorithm in readme
-- TODO git(hub,lab) flow or something?
-- TODO 'mit branch' with dirty working directory - apply changes to new worktree?
-- TODO undo in more cases?
-- TODO recommend merging master if it conflicts

main :: IO ()
main = do
  getArgs >>= \case
    ["branch", branch] -> mitBranch (Text.pack branch)
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> mitClone url name
    ["commit"] -> mitCommit
    ["merge", branch] -> mitMerge (Text.pack branch)
    ["sync"] -> mitSync
    ["undo"] -> mitUndo
    _ -> do
      putLines
        [ "Usage:",
          "  mit branch ≪branch≫",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit merge ≪branch≫",
          "  mit sync",
          "  mit undo"
        ]
      exitFailure

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

  unlessM (doesDirectoryExist (Text.unpack worktreeDir)) do
    worktrees :: [(Text, Text, Maybe Text)] <-
      gitWorktreeList

    case List.find (\(_, _, worktreeBranch) -> worktreeBranch == Just branch) worktrees of
      Nothing -> do
        git_ ["worktree", "add", "--detach", worktreeDir]
        withCurrentDirectory (Text.unpack worktreeDir) do
          git ["rev-parse", "--verify", "refs/heads/" <> branch] >>= \case
            False -> do
              git_ ["branch", "--no-track", branch]
              git_ ["switch", branch]

              _fetchResult :: Bool <-
                git ["fetch", "origin"]

              whenM (git ["rev-parse", "--verify", "refs/remotes/" <> upstream]) do
                git_ ["reset", "--hard", upstream]
                git_ ["branch", "--set-upstream-to", upstream]
            True -> git_ ["switch", branch]
      Just (dir, _, _) ->
        unless (worktreeDir == dir) do
          Text.putStrLn ("Branch " <> Text.bold branch <> " is already checked out in " <> Text.bold dir)
          exitFailure
  where
    upstream :: Text
    upstream =
      "origin/" <> branch

    worktreeDir :: Text
    worktreeDir =
      Text.dropWhileEnd (/= '/') gitdir <> branch

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
      Just upstreamHead -> gitCommitsBetween (Just context.head) upstreamHead

  localCommits0 :: [GitCommitInfo] <-
    gitCommitsBetween maybeUpstreamHead "HEAD"

  when (not (null remoteCommits) && null localCommits0) do
    readCommitFile context.branch64 >>= \case
      Just (hash, age) | hash == context.head && age < 10_000_000_000 -> pure ()
      _ -> do
        recordCommitFile context.branch64 context.head
        putLines
          [ "",
            "  " <> Text.italic context.branch <> " is not up to date.",
            "",
            "  Run " <> Text.bold (Text.blue "mit sync") <> " first, or run " <> Text.bold (Text.blue "mit commit")
              <> " again to record a commit anyway.",
            ""
          ]
        exitFailure

  stash :: Text <-
    gitCreateStash

  commitResult :: Bool <-
    gitCommit

  -- If the commit was aborted, reset the timer (allowing another 'mit commit' right away). Otherwise, delete the commit
  -- file, because it's no longer needed.
  case commitResult of
    False -> recordCommitFile context.branch64 context.head
    True -> deleteCommitFile context.branch64

  localCommits :: [GitCommitInfo] <-
    case commitResult of
      False -> pure localCommits0
      True -> gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult :: PushResult <-
    if
        | null localCommits -> pure (PushNotAttempted PushNoCommits)
        | not (null remoteCommits) -> pure (PushNotAttempted PushWouldConflict)
        | context.fetchFailed -> pure (PushNotAttempted PushOffline)
        | otherwise -> PushAttempted <$> gitPush context.branch

  canUndo :: Bool <- do
    let pushDidntHappen = do
          head <- git ["rev-parse", "HEAD"]
          case head == context.head of
            False -> do
              recordUndoFile context.branch64 [Reset context.head, Apply stash]
              pure True
            True -> pure False
    case pushResult of
      PushAttempted True ->
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
      PushAttempted False -> pushDidntHappen
      PushNotAttempted _ -> pushDidntHappen

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        conflicts = [],
        syncs =
          [ Sync
              { commits = localCommits,
                result = pushResultToSyncResult pushResult,
                source = context.branch,
                target = context.upstream
              }
          ]
      }

data PushResult
  = PushAttempted Bool
  | PushNotAttempted PushNotAttemptedReason

data PushNotAttemptedReason
  = PushCommitHasConflicts -- local commit has conflict markers
  | PushNewCommits -- we just pulled remote commits; don't push in case there's something local to address
  | PushNoCommits -- no commits to push
  | PushOffline -- fetch failed, so we seem offline
  | PushWouldConflict -- local history has forked, need to sync

pushResultToSyncResult :: PushResult -> SyncResult
pushResultToSyncResult = \case
  PushAttempted True -> Success
  PushAttempted False -> Failure
  PushNotAttempted PushCommitHasConflicts -> Failure
  PushNotAttempted PushNewCommits -> Pending
  PushNotAttempted PushNoCommits -> Success -- doesnt matter, wont be shown
  PushNotAttempted PushOffline -> Offline
  PushNotAttempted PushWouldConflict -> Failure

mitMerge :: Text -> IO ()
mitMerge target0 = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context <- makeContext

  -- When given 'mit merge foo', prefer merging 'origin/foo' over 'foo'
  target <- do
    let remote = "origin/" <> target0
    git ["rev-parse", "--verify", remote] <&> \case
      False -> target0
      True -> remote

  (targetCommits, mergeConflicts, stashConflicts, canUndo) <-
    git ["rev-parse", target] >>= \case
      Left _ -> pure ([], [], [], False)
      Right targetCommit ->
        gitCommitsBetween (Just context.head) targetCommit >>= \case
          [] -> pure ([], [], [], False)
          targetCommits -> do
            maybeStash <-
              case context.dirty of
                Differences -> Just <$> gitStash
                NoDifferences -> pure Nothing
            mergeConflicts <- gitMerge' context.branch target
            stashConflicts <-
              case mergeConflicts of
                [] ->
                  case maybeStash of
                    Nothing -> pure []
                    Just stash -> gitApplyStash stash
                _ -> pure []
            recordUndoFile context.branch64 (Reset context.head : maybeToList (Apply <$> maybeStash))
            pure (targetCommits, mergeConflicts, stashConflicts, True)

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        -- At most one of stash/merge conflicts is non-null, so (++)-ing then makes sense
        conflicts = stashConflicts ++ mergeConflicts,
        syncs =
          [ Sync
              { commits = targetCommits,
                result = if null mergeConflicts then Success else Failure,
                source = target,
                target = context.branch
              }
          ]
      }

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
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
      Just upstreamHead -> gitCommitsBetween (Just context.head) upstreamHead

  maybeStash :: Maybe Text <-
    case (remoteCommits, context.dirty) of
      (_ : _, Differences) -> Just <$> gitStash
      _ -> pure Nothing

  mergeConflicts :: Maybe [GitConflict] <-
    case remoteCommits of
      [] -> pure Nothing
      _ ->
        fmap Just do
          gitMerge context.branch context.upstream >>= \case
            Left commitConflicts -> commitConflicts
            Right () -> pure []

  localCommits :: [GitCommitInfo] <-
    gitCommitsBetween maybeUpstreamHead "HEAD"

  stashConflicts :: [GitConflict] <-
    case maybeStash of
      Nothing -> pure []
      Just stash -> gitApplyStash stash

  pushResult :: PushResult <-
    case localCommits of
      [] -> pure (PushNotAttempted PushNoCommits)
      _ ->
        case mergeConflicts of
          Nothing ->
            case context.fetchFailed of
              False -> PushAttempted <$> gitPush context.branch
              True -> pure (PushNotAttempted PushOffline)
          Just [] -> pure (PushNotAttempted PushNewCommits)
          Just (_ : _) -> pure (PushNotAttempted PushWouldConflict)

  canUndo :: Bool <- do
    let pushDidntHappen = do
          head2 <- git ["rev-parse", "HEAD"]
          if context.head == head2
            then pure False -- head didn't move; nothing to undo
            else do
              recordUndoFile context.branch64 (Reset context.head : maybeToList (Apply <$> maybeStash))
              pure True
    case pushResult of
      PushAttempted True -> do
        deleteUndoFile context.branch64
        pure False
      PushAttempted False -> pushDidntHappen
      PushNotAttempted _ -> pushDidntHappen

  putSummary
    Summary
      { branch = context.branch,
        canUndo,
        -- FIXME this is dubious, nub made more sense when conflicts were just filenames...
        conflicts = List.nub (stashConflicts ++ fromMaybe [] mergeConflicts),
        syncs =
          [ Sync
              { commits = remoteCommits,
                result =
                  case mergeConflicts of
                    Just (_ : _) -> Failure
                    _ -> Success,
                source = context.upstream,
                target = context.branch
              },
            Sync
              { commits = localCommits,
                result = pushResultToSyncResult pushResult,
                source = context.branch,
                target = context.upstream
              }
          ]
      }

-- FIXME output what we just undid
mitUndo :: IO ()
mitUndo = do
  dieIfNotInGitDir

  branch64 <- Text.encodeBase64 <$> gitCurrentBranch
  readUndoFile branch64 >>= \case
    Nothing -> exitFailure
    Just undos -> do
      deleteUndoFile branch64
      for_ undos \case
        Apply commit -> do
          -- This should never return conflicts
          -- FIXME assert?
          _conflicts <- gitApplyStash commit
          pure ()
        Reset commit -> gitResetHard commit
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
  branch <- gitCurrentBranch
  dirty <- gitDiff
  head <- git ["rev-parse", "HEAD"]
  fetchFailed <- not <$> git ["fetch", "origin"]
  pure
    Context
      { branch,
        branch64 = Text.encodeBase64 branch,
        dirty,
        fetchFailed,
        head,
        upstream = "origin/" <> branch
      }

data Summary = Summary
  { branch :: Text,
    canUndo :: Bool,
    conflicts :: [GitConflict],
    syncs :: [Sync]
  }

data Sync = Sync
  { commits :: [GitCommitInfo],
    result :: SyncResult,
    source :: Text,
    target :: Text
  }

data SyncResult
  = Offline
  | Failure
  | Pending
  | Success

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
        else
          "  The following files have conflicts." :
          map (("    " <>) . Text.red . showGitConflict) summary.conflicts ++ [""]
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
          case sync.result of
            Offline -> Text.brightBlack
            Failure -> Text.red
            Pending -> Text.yellow
            Success -> Text.green
    undoLines :: [Text]
    undoLines =
      if summary.canUndo
        then ["  Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change.", ""]
        else []

-- Commit file utils

deleteCommitFile :: Text -> IO ()
deleteCommitFile branch64 =
  removeFile (commitfile branch64) `catch` \(_ :: IOException) -> pure ()

-- | Read the amount of time that has elapsed (in nanoseconds) since the commit file was created
readCommitFile :: Text -> IO (Maybe (Text, Integer))
readCommitFile branch64 = do
  try (Text.readFile (commitfile branch64)) >>= \case
    Left (_ :: IOException) -> pure Nothing
    Right contents ->
      case Text.split (== ',') contents of
        [hash, text2int -> Just t0] -> do
          t1 <- Clock.getTime Clock.Realtime
          pure (Just (hash, Clock.toNanoSecs t1 - t0))
        _ -> do
          deleteCommitFile branch64
          pure Nothing
  where
    -- FIXME make this faster
    text2int :: Text -> Maybe Integer
    text2int =
      readMaybe . Text.unpack

recordCommitFile :: Text -> Text -> IO ()
recordCommitFile branch64 hash = do
  now <- Clock.getTime Clock.Realtime
  -- FIXME use builder
  let contents = hash <> "," <> int2text (Clock.toNanoSecs now)
  Text.writeFile (commitfile branch64) contents `catch` \(_ :: IOException) -> pure ()
  where
    -- FIXME make this faster
    int2text :: Integer -> Text
    int2text =
      Text.pack . show

commitfile :: Text -> FilePath
commitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64 <> "-commit")

-- Undo file utils

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

deleteUndoFile :: Text -> IO ()
deleteUndoFile branch64 =
  removeFile (undofile branch64) `catch` \(_ :: IOException) -> pure ()

readUndoFile :: Text -> IO (Maybe [Undo])
readUndoFile branch64 =
  try (Text.readFile file) >>= \case
    Left (_ :: IOException) -> pure Nothing
    Right contents ->
      case parseUndos contents of
        Nothing -> do
          when debug (putStrLn ("Corrupt undo file: " ++ file))
          pure Nothing
        Just undos -> pure (Just undos)
  where
    file :: FilePath
    file =
      undofile branch64

-- Record a file for 'mit undo' to use, if invoked.
recordUndoFile :: Text -> [Undo] -> IO ()
recordUndoFile branch64 undos = do
  Text.writeFile (undofile branch64) (showUndos undos) `catch` \(_ :: IOException) -> pure ()

undofile :: Text -> FilePath
undofile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)

-- Globals

debug :: Bool
debug =
  isJust (unsafePerformIO (lookupEnv "debug"))
{-# NOINLINE debug #-}

gitdir :: Text
gitdir =
  unsafePerformIO (git ["rev-parse", "--absolute-git-dir"])
{-# NOINLINE gitdir #-}

-- Git helpers

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

-- FIXME document what this does
gitMerge' :: Text -> Text -> IO [GitConflict]
gitMerge' me target = do
  git ["merge", "--ff", "--no-commit", target] >>= \case
    False -> gitConflicts
    True -> do
      whenM gitMergeInProgress (git_ ["commit", "--message", mergeMessage []])
      pure []
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

-- | Create a stash and blow away local changes (like 'git stash push')
gitStash :: IO Text
gitStash = do
  stash <- gitCreateStash
  gitResetHard "HEAD"
  pure stash

gitUnstageChanges :: IO ()
gitUnstageChanges = do
  git_ ["reset"]
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
      Text.bold (Text.brightBlack (Text.unwords (marker <> " git" : map quoteText args)))
        : map (Text.brightBlack . ("    " <>)) (stdoutLines ++ stderrLines)
  where
    marker :: Text
    marker =
      case exitCode of
        ExitFailure _ -> "✗"
        ExitSuccess -> "✓"

-- Mini prelude extensions

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
