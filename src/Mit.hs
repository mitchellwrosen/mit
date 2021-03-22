{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Mit where

import Control.Category ((>>>))
import Control.Exception (AsyncException (UserInterrupt), IOException, catch, evaluate, throwIO, try)
import Control.Monad
import Data.Char
import Data.Foldable (fold, for_)
import Data.Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as List1
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import Data.Traversable
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
-- TODO mit log
-- TODO optparse-applicative

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
  whenM gitExistUntrackedFiles dieIfBuggyGit
  gitDiff >>= \case
    Differences -> pure ()
    NoDifferences -> exitFailure
  context <- makeContext
  gitMergeInProgress >>= \case
    False -> mitCommitWith context
    True -> mitCommitMerge context

mitCommitMerge :: Context -> IO ()
mitCommitMerge context = do
  maybeState0 <- readMitState context
  let maybeMerging = do
        state0 <- maybeState0
        state0.merging
  case maybeMerging of
    Nothing -> git_ ["commit", "--all", "--no-edit"]
    Just merging ->
      let message =
            fold
              [ "⅄ ",
                if merging == context.branch then "" else (merging <> " → "),
                context.branch
              ]
       in git_ ["commit", "--all", "--message", message]
  let undos = maybe [] (.undos) maybeState0
  case [commit | Apply commit <- undos] of
    [] -> mitSyncWith context (Just [Reset context.head])
    stash : _ ->
      gitApplyStash stash >>= \case
        -- FIXME we just unstashed, now we're about to stash again :/
        [] -> mitSyncWith context (Just [Reset context.head, Apply stash])
        stashConflicts -> do
          head <- git ["rev-parse", "HEAD"]
          writeMitState context MitState {head, merging = Nothing, ranCommitAt = Nothing, undos}
          putSummary
            Summary
              { branch = context.branch,
                canUndo = not (null undos),
                conflicts = stashConflicts,
                syncs = []
              }

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

  localCommits0 <- gitCommitsBetween maybeUpstreamHead "HEAD"
  maybeState0 <- readMitState context

  shouldWarnAboutFork <- do
    let wouldFork = not (null remoteCommits) && null localCommits0
    case wouldFork of
      False -> pure False
      True -> do
        let theyRanMitCommitRecently =
              case maybeState0 of
                Just MitState {ranCommitAt = Just t0} -> do
                  t1 <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
                  pure ((t1 - t0) < 10_000_000_000)
                _ -> pure False
        not <$> theyRanMitCommitRecently

  case shouldWarnAboutFork of
    False -> do
      stash <- gitCreateStash
      commitResult <- gitCommit
      localCommits <- if commitResult then gitCommitsBetween maybeUpstreamHead "HEAD" else pure localCommits0

      pushResult <-
        if
            | null localCommits -> pure (PushNotAttempted PushNoCommits)
            | not (null remoteCommits) -> pure (PushNotAttempted PushWouldConflict)
            | context.fetchFailed -> pure (PushNotAttempted PushOffline)
            | otherwise -> PushAttempted <$> gitPush context.branch

      state1 <- do
        ranCommitAt <-
          case commitResult of
            False -> Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
            True -> pure Nothing
        undos <-
          let onDidntPush =
                case commitResult of
                  False -> maybe [] (.undos) maybeState0
                  True -> [Reset context.head, Apply stash]
           in case pushResult of
                PushAttempted True ->
                  case (commitResult, localCommits) of
                    (True, [_]) -> do
                      head <- git ["rev-parse", "HEAD"]
                      pure [Revert head, Apply stash]
                    _ -> pure []
                PushAttempted False -> pure onDidntPush
                PushNotAttempted _ -> pure onDidntPush
        pure MitState {head = (), merging = Nothing, ranCommitAt, undos}

      writeMitState context state1

      putSummary
        Summary
          { branch = context.branch,
            -- Whether we "can undo" from here is not exactly if the state says we can undo, because of one corner case:
            -- we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
            --
            -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit'
            -- was never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as
            -- if it would undo the last command run, namely the 'mit commit' that was aborted.
            canUndo = not (null state1.undos) && commitResult,
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
    True -> do
      ranCommitAt <- Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
      let state0 =
            case maybeState0 of
              Nothing -> MitState {head = (), merging = Nothing, ranCommitAt, undos = []}
              Just s0 -> s0 {ranCommitAt}
      writeMitState context state0
      putLines
        [ "",
          "  " <> Text.italic context.branch <> " is not up to date.",
          "",
          "  Run " <> Text.bold (Text.blue "mit sync") <> " first, or run " <> Text.bold (Text.blue "mit commit")
            <> " again to record a commit anyway.",
          ""
        ]
      exitFailure

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

-- FIXME if on branch 'foo', handle 'mitMerge foo' or 'mitMerge origin/foo' as 'mitSync'?
mitMerge :: Text -> IO ()
mitMerge target = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  context <- makeContext

  -- When given 'mit merge foo', prefer merging 'origin/foo' over 'foo'
  targetCommit <- do
    git ["rev-parse", "origin/" <> target] >>= \case
      Left _ ->
        git ["rev-parse", target] >>= \case
          Left _ -> exitFailure
          Right commit -> pure commit
      Right commit -> pure commit

  mergeStatus <- mitMerge' ("⅄ " <> target <> " → " <> context.branch) targetCommit

  writeMitState
    context
    MitState
      { head = (),
        merging =
          case mergeStatus of
            MergeStatus'Merge _ (MergeResult'MergeConflicts _) _ -> Just target
            MergeStatus'Merge _ (MergeResult'StashConflicts _) _ -> Nothing
            MergeStatus'NoMerge -> Nothing,
        ranCommitAt = Nothing,
        undos =
          case mergeStatus of
            MergeStatus'Merge _ _ undos -> undos
            MergeStatus'NoMerge -> []
      }

  putSummary
    Summary
      { branch = context.branch,
        canUndo =
          case mergeStatus of
            MergeStatus'Merge _ _ _ -> True
            MergeStatus'NoMerge -> False,
        -- At most one of stash/merge conflicts is non-null, so just appending them makes some sense
        conflicts =
          case mergeStatus of
            MergeStatus'Merge _commits result _undos ->
              case result of
                MergeResult'MergeConflicts conflicts -> List1.toList conflicts
                MergeResult'StashConflicts conflicts -> conflicts
            MergeStatus'NoMerge -> [],
        syncs =
          case mergeStatus of
            MergeStatus'NoMerge -> []
            MergeStatus'Merge commits result _undos ->
              [ Sync
                  { commits = List1.toList commits,
                    result =
                      case result of
                        MergeResult'MergeConflicts _ -> Failure
                        -- Even if we have conflicts from unstashing, we call this merge a success.
                        MergeResult'StashConflicts _ -> Success,
                    source = target,
                    target = context.branch
                  }
              ]
      }

data MergeStatus
  = MergeStatus'NoMerge
  | MergeStatus'Merge (List1 GitCommitInfo) MergeResult [Undo]

data MergeResult
  = MergeResult'MergeConflicts (List1 GitConflict)
  | MergeResult'StashConflicts [GitConflict]

mitMerge' :: Text -> Text -> IO MergeStatus
mitMerge' message target = do
  head <- git ["rev-parse", "HEAD"]
  (List1.nonEmpty <$> gitCommitsBetween (Just head) target) >>= \case
    Nothing -> pure MergeStatus'NoMerge
    Just commits -> do
      maybeStash <- gitStash
      let undos = Reset head : maybeToList (Apply <$> maybeStash)
      result <-
        git ["merge", "--ff", "--no-commit", target] >>= \case
          False -> do
            conflicts <- gitConflicts
            pure (MergeResult'MergeConflicts (List1.fromList conflicts))
          True -> do
            whenM gitMergeInProgress (git_ ["commit", "--message", message])
            maybeConflicts <- for maybeStash gitApplyStash
            pure (MergeResult'StashConflicts (fromMaybe [] maybeConflicts))
      pure (MergeStatus'Merge commits result undos)

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: IO ()
mitSync = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit
  context <- makeContext
  mitSyncWith context Nothing

-- | @mitSyncWith context maybeUndos@
--
-- Whenever recording what 'mit undo' should do after 'mit sync', if 'maybeUndos' is provided, we use them instead.
-- This is pulled into a function argument to get better undo behavior after committing a merge.
--
-- Consider:
--
-- The user runs 'mit merge foo' (with or without a clean working tree), and gets conflicts. After fixing them, she runs
-- 'mit commit'. This may result in *additional* conflicts due to the just-stashed uncommitted changes.
--
-- But either way, internally, we would like this 'mit commit' to effectively behave as a normal commit, in the sense
-- that we want to immediately push it upstream. That means the code would like to simply call 'mit sync' after
-- 'git commit'!
--
-- However, if this commit could be undone (i.e. we didn't push it), we wouldn't want that 'mit sync' to *locally*
-- compute where to undo, because it would just conclude, "oh, HEAD hasn't moved, and we didn't push, so there's nothing
-- to undo".
--
-- Instead, we want to undo to the point before running the 'mit merge' that caused the conflicts, which were later
-- resolved by 'mit commit'.
mitSyncWith :: Context -> Maybe [Undo] -> IO ()
mitSyncWith context maybeUndos = do
  maybeUpstreamHead :: Maybe Text <-
    git ["rev-parse", "refs/remotes/" <> context.upstream] <&> \case
      Left _ -> Nothing
      Right upstreamHead -> Just upstreamHead

  maybeMergeStatus <- for maybeUpstreamHead (mitMerge' ("⅄ " <> context.branch))

  localCommits :: [GitCommitInfo] <-
    gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult :: PushResult <-
    case localCommits of
      [] -> pure (PushNotAttempted PushNoCommits)
      _ ->
        case maybeMergeStatus of
          Nothing ->
            case context.fetchFailed of
              False -> PushAttempted <$> gitPush context.branch
              True -> pure (PushNotAttempted PushOffline)
          Just MergeStatus'NoMerge ->
            case context.fetchFailed of
              False -> PushAttempted <$> gitPush context.branch
              True -> pure (PushNotAttempted PushOffline)
          Just (MergeStatus'Merge _commits result _undos) ->
            case result of
              MergeResult'MergeConflicts _conflicts -> pure (PushNotAttempted PushWouldConflict) -- FIXME better reason
              MergeResult'StashConflicts _conflicts -> pure (PushNotAttempted PushNewCommits)

  let undos =
        let undoMerge =
              case maybeMergeStatus of
                Nothing -> []
                Just MergeStatus'NoMerge -> []
                Just (MergeStatus'Merge _commits _result us) -> us
         in case pushResult of
              PushAttempted True -> []
              PushAttempted False -> fromMaybe undoMerge maybeUndos
              PushNotAttempted _ -> fromMaybe undoMerge maybeUndos

  writeMitState
    context
    MitState
      { head = (),
        merging =
          case maybeMergeStatus of
            Nothing -> Nothing
            Just (MergeStatus'Merge _ (MergeResult'MergeConflicts _) _) -> Just context.branch
            Just (MergeStatus'Merge _ (MergeResult'StashConflicts _) _) -> Nothing
            Just MergeStatus'NoMerge -> Nothing,
        ranCommitAt = Nothing,
        undos
      }

  putSummary
    Summary
      { branch = context.branch,
        canUndo = not (null undos),
        conflicts =
          case maybeMergeStatus of
            Nothing -> []
            Just (MergeStatus'Merge _commits result _undos) ->
              case result of
                MergeResult'MergeConflicts conflicts -> List1.toList conflicts
                MergeResult'StashConflicts conflicts -> conflicts
            Just MergeStatus'NoMerge -> [],
        syncs =
          [ Sync
              { commits =
                  case maybeMergeStatus of
                    Nothing -> []
                    Just MergeStatus'NoMerge -> []
                    Just (MergeStatus'Merge commits _result _undos) -> List1.toList commits,
                result =
                  case maybeMergeStatus of
                    Nothing -> Success
                    Just MergeStatus'NoMerge -> Success
                    Just (MergeStatus'Merge _commits result _undos) ->
                      case result of
                        MergeResult'MergeConflicts _ -> Failure
                        MergeResult'StashConflicts _ -> Success,
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
    fetchFailed :: Bool,
    head :: Text,
    upstream :: Text
  }

makeContext :: IO Context
makeContext = do
  branch <- gitCurrentBranch
  head <- git ["rev-parse", "HEAD"]
  fetchFailed <- not <$> git ["fetch", "origin"]
  pure
    Context
      { branch,
        branch64 = Text.encodeBase64 branch,
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

-- FIXME consolidate these files

-- State file

data MitState a = MitState
  { head :: a,
    merging :: Maybe Text,
    ranCommitAt :: Maybe Integer,
    undos :: [Undo]
  }

deleteMitState :: Context -> IO ()
deleteMitState context =
  removeFile (mitfile context.branch64) `catch` \(_ :: IOException) -> pure ()

parseMitState :: Text -> Maybe (MitState Text)
parseMitState contents = do
  [headLine, mergingLine, ranCommitAtLine, undosLine] <- Just (Text.lines contents)
  ["head", head] <- Just (Text.words headLine)
  merging <-
    case Text.words mergingLine of
      ["merging"] -> Just Nothing
      ["merging", branch] -> Just (Just branch)
      _ -> Nothing
  ranCommitAt <-
    case Text.words ranCommitAtLine of
      ["committed-at"] -> Just Nothing
      ["committed-at", text2int -> Just n] -> Just (Just n)
      _ -> Nothing
  undos <- Text.stripPrefix "undos " undosLine >>= parseUndos
  pure MitState {head, merging, ranCommitAt, undos}

readMitState :: Context -> IO (Maybe (MitState ()))
readMitState context =
  try (Text.readFile (mitfile context.branch64)) >>= \case
    Left (_ :: IOException) -> pure Nothing
    Right contents -> do
      let maybeState = do
            state <- parseMitState contents
            guard (context.head == state.head)
            pure state
      case maybeState of
        Nothing -> do
          deleteMitState context
          pure Nothing
        Just state -> pure (Just (state {head = ()} :: MitState ()))

writeMitState :: Context -> MitState () -> IO ()
writeMitState context state = do
  head <- git ["rev-parse", "HEAD"]
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "ran-commit-at " <> maybe Text.empty int2text state.ranCommitAt,
            "undos " <> showUndos state.undos
          ]
  Text.writeFile (mitfile context.branch64) contents `catch` \(_ :: IOException) -> pure ()

mitfile :: Text -> FilePath
mitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)

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

-- FIXME this finds the wrong dir for worktrees
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

-- Mini prelude extensions

type List1 =
  List1.NonEmpty

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

-- FIXME make this faster
int2text :: Integer -> Text
int2text =
  Text.pack . show

putLines :: [Text] -> IO ()
putLines =
  Text.putStr . Text.unlines

quoteText :: Text -> Text
quoteText s =
  if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s

-- FIXME make this faster
text2int :: Text -> Maybe Integer
text2int =
  readMaybe . Text.unpack

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
