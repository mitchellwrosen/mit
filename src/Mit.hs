{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Mit where

import qualified Data.List.NonEmpty as List1
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import Mit.Git
import Mit.Prelude
import qualified System.Clock as Clock
import System.Directory (doesDirectoryExist, removeFile, withCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)

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

  gitBranchWorktreeDir branch >>= \case
    Nothing ->
      doesDirectoryExist (Text.unpack worktreeDir) >>= \case
        False -> do
          git_ ["worktree", "add", "--detach", worktreeDir]
          withCurrentDirectory (Text.unpack worktreeDir) do
            gitBranchExists branch >>= \case
              False -> do
                gitBranch branch
                gitSwitch branch
                gitFetch_ "origin"
                whenM (gitRemoteBranchExists "origin" branch) do
                  git_ ["reset", "--hard", upstream]
                  git_ ["branch", "--set-upstream-to", upstream]
              True -> gitSwitch branch
        True -> die ["Directory " <> Text.bold worktreeDir <> " already exists."]
    Just directory ->
      unless (directory == worktreeDir) do
        die [Text.bold branch <> " is already checked out in " <> Text.bold directory <> "."]
  where
    upstream :: Text
    upstream =
      "origin/" <> branch

    worktreeDir :: Text
    worktreeDir =
      Text.dropWhileEnd (/= '/') gitdir <> branch

mitClone :: Text -> Text -> IO ()
mitClone url name =
  -- FIXME use 'git config --get init.defaultBranch'
  git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]

mitCommit :: IO ()
mitCommit = do
  dieIfNotInGitDir
  whenM gitExistUntrackedFiles dieIfBuggyGit
  gitDiff >>= \case
    Differences ->
      gitMergeInProgress >>= \case
        False -> mitCommit_
        True -> mitCommitMerge
    NoDifferences -> exitFailure

mitCommit_ :: IO ()
mitCommit_ = do
  fetched <- git ["fetch", "origin"]
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  head <- gitHead
  maybeUpstreamHead <- gitRemoteBranchHead "origin" branch
  existRemoteCommits <- maybe (pure False) (gitExistCommitsBetween head) maybeUpstreamHead
  existLocalCommits <- maybe (pure True) (\upstreamHead -> gitExistCommitsBetween upstreamHead "HEAD") maybeUpstreamHead
  maybeState0 <- readMitState branch64

  let wouldFork = existRemoteCommits && not existLocalCommits
  shouldWarnAboutFork <- do
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
      committed <- gitCommit
      localCommits <- gitCommitsBetween maybeUpstreamHead "HEAD"

      pushResult <-
        if
            | null localCommits -> pure (PushNotAttempted PushNoCommits)
            | existRemoteCommits -> pure (PushNotAttempted PushWouldConflict)
            | not fetched -> pure (PushNotAttempted PushOffline)
            | otherwise -> PushAttempted <$> gitPush branch

      let pushed =
            case pushResult of
              PushAttempted success -> success
              PushNotAttempted _ -> False

      -- Only bother resetting the "ran commit at" if we would fork and the commit was aborted
      ranCommitAt <-
        case (wouldFork, committed) of
          (True, False) -> Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
          _ -> pure Nothing

      undos <-
        case (pushed, committed, localCommits) of
          (False, False, _) -> pure (maybe [] (.undos) maybeState0)
          (False, True, _) -> pure [Reset head, Apply stash]
          (True, True, [_]) -> do
            head1 <- gitHead
            pure [Revert head1, Apply stash]
          (True, _, _) -> pure []

      writeMitState branch64 MitState {head = (), merging = Nothing, ranCommitAt, undos}

      putSummary
        Summary
          { branch,
            -- Whether we "can undo" from here is not exactly if the state says we can undo, because of one corner case:
            -- we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
            --
            -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit'
            -- was never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as
            -- if it would undo the last command run, namely the 'mit commit' that was aborted.
            canUndo = not (null undos) && committed,
            conflicts = [],
            syncs =
              case List1.nonEmpty localCommits of
                Nothing -> []
                Just commits ->
                  [ Sync
                      { commits,
                        result = pushResultToSyncResult pushResult,
                        source = branch,
                        target = "origin/" <> branch
                      }
                  ]
          }
    True -> do
      ranCommitAt <- Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
      let state0 =
            case maybeState0 of
              Nothing -> MitState {head = (), merging = Nothing, ranCommitAt, undos = []}
              Just s0 -> s0 {ranCommitAt}
      writeMitState branch64 state0
      putLines
        [ "",
          "  " <> Text.italic branch <> " is not up to date.",
          "",
          "  Run " <> Text.bold (Text.blue "mit sync") <> " first, or run " <> Text.bold (Text.blue "mit commit")
            <> " again to record a commit anyway.",
          ""
        ]
      exitFailure

mitCommitMerge :: IO ()
mitCommitMerge = do
  context <- makeContext
  maybeState0 <- readMitState context.branch64
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
          writeMitState context.branch64 MitState {head = (), merging = Nothing, ranCommitAt = Nothing, undos}
          putSummary
            Summary
              { branch = context.branch,
                canUndo = not (null undos),
                conflicts = stashConflicts,
                syncs = []
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

-- FIXME if on branch 'foo', handle 'mitMerge foo' or 'mitMerge origin/foo' as 'mitSync'?
mitMerge :: Text -> IO ()
mitMerge target = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit

  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch

  -- When given 'mit merge foo', prefer merging 'origin/foo' over 'foo'
  targetCommit <- do
    _fetched <- gitFetch "origin"
    gitRemoteBranchHead "origin" target & onNothingM (git ["rev-parse", target] & onLeftM \_ -> exitFailure)

  maybeMergeStatus <- mitMerge' ("⅄ " <> target <> " → " <> branch) targetCommit

  writeMitState
    branch64
    MitState
      { head = (),
        merging = do
          mergeStatus <- maybeMergeStatus
          case mergeStatus.result of
            MergeResult'MergeConflicts _ -> Just target
            MergeResult'StashConflicts _ -> Nothing,
        ranCommitAt = Nothing,
        undos =
          case maybeMergeStatus of
            Nothing -> []
            Just mergeStatus -> mergeStatus.undos
      }

  putSummary
    Summary
      { branch,
        canUndo = isJust maybeMergeStatus,
        conflicts =
          case maybeMergeStatus of
            Nothing -> []
            Just mergeStatus ->
              case mergeStatus.result of
                MergeResult'MergeConflicts conflicts -> List1.toList conflicts
                MergeResult'StashConflicts conflicts -> conflicts,
        syncs = do
          mergeStatus <- maybeToList maybeMergeStatus
          pure
            Sync
              { commits = mergeStatus.commits,
                result =
                  case mergeStatus.result of
                    MergeResult'MergeConflicts _ -> Failure
                    -- Even if we have conflicts from unstashing, we call this merge a success.
                    MergeResult'StashConflicts _ -> Success,
                source = target,
                target = branch
              }
      }

data MergeStatus = MergeStatus
  { commits :: List1 GitCommitInfo,
    result :: MergeResult,
    undos :: [Undo] -- FIXME List1
  }

data MergeResult
  = MergeResult'MergeConflicts (List1 GitConflict)
  | MergeResult'StashConflicts [GitConflict]

mitMerge' :: Text -> Text -> IO (Maybe MergeStatus)
mitMerge' message target = do
  head <- gitHead
  (List1.nonEmpty <$> gitCommitsBetween (Just head) target) >>= \case
    Nothing -> pure Nothing
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
      pure (Just MergeStatus {commits, result, undos})

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

  maybeMergeStatus <-
    case maybeUpstreamHead of
      Nothing -> pure Nothing
      Just upstreamHead -> mitMerge' ("⅄ " <> context.branch) upstreamHead

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
          Just mergeStatus ->
            case mergeStatus.result of
              MergeResult'MergeConflicts _conflicts -> pure (PushNotAttempted PushWouldConflict) -- FIXME better reason
              MergeResult'StashConflicts _conflicts -> pure (PushNotAttempted PushNewCommits)

  let undos =
        let undoMerge =
              case maybeMergeStatus of
                Nothing -> []
                Just mergeStatus -> mergeStatus.undos
         in case pushResult of
              PushAttempted True -> []
              PushAttempted False -> fromMaybe undoMerge maybeUndos
              PushNotAttempted _ -> fromMaybe undoMerge maybeUndos

  writeMitState
    context.branch64
    MitState
      { head = (),
        merging = do
          mergeStatus <- maybeMergeStatus
          case mergeStatus.result of
            MergeResult'MergeConflicts _ -> Just context.branch
            MergeResult'StashConflicts _ -> Nothing,
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
            Just mergeStatus ->
              case mergeStatus.result of
                MergeResult'MergeConflicts conflicts -> List1.toList conflicts
                MergeResult'StashConflicts conflicts -> conflicts,
        syncs =
          catMaybes
            [ do
                mergeStatus <- maybeMergeStatus
                pure
                  Sync
                    { commits = mergeStatus.commits,
                      result =
                        case mergeStatus.result of
                          MergeResult'MergeConflicts _ -> Failure
                          MergeResult'StashConflicts _ -> Success,
                      source = context.upstream,
                      target = context.branch
                    },
              do
                commits <- List1.nonEmpty localCommits
                pure
                  Sync
                    { commits,
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

  readMitState branch64 >>= \case
    Nothing -> exitFailure
    Just MitState {undos} -> do
      case List1.nonEmpty undos of
        Nothing -> exitFailure
        Just undos1 -> applyUndos undos1
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
  head <- gitHead
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
  { commits :: List1 GitCommitInfo,
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
      colorize (Text.italic ("  " <> sync.source <> " → " <> sync.target)) :
      map (("  " <>) . prettyGitCommitInfo) (List1.toList sync.commits)
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

-- State file

data MitState a = MitState
  { head :: a,
    merging :: Maybe Text,
    ranCommitAt :: Maybe Integer,
    undos :: [Undo]
  }
  deriving stock (Eq, Show)

deleteMitState :: Text -> IO ()
deleteMitState branch64 =
  removeFile (mitfile branch64) `catch` \(_ :: IOException) -> pure ()

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
      ["ran-commit-at"] -> Just Nothing
      ["ran-commit-at", text2int -> Just n] -> Just (Just n)
      _ -> Nothing
  undos <- Text.stripPrefix "undos " undosLine >>= parseUndos
  pure MitState {head, merging, ranCommitAt, undos}

readMitState :: Text -> IO (Maybe (MitState ()))
readMitState branch64 = do
  head <- gitHead
  try (Text.readFile (mitfile branch64)) >>= \case
    Left (_ :: IOException) -> pure Nothing
    Right contents -> do
      let maybeState = do
            state <- parseMitState contents
            guard (head == state.head)
            pure state
      case maybeState of
        Nothing -> do
          deleteMitState branch64
          pure Nothing
        Just state -> pure (Just (state {head = ()} :: MitState ()))

writeMitState :: Text -> MitState () -> IO ()
writeMitState branch64 state = do
  head <- gitHead
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "ran-commit-at " <> maybe Text.empty int2text state.ranCommitAt,
            "undos " <> showUndos state.undos
          ]
  Text.writeFile (mitfile branch64) contents `catch` \(_ :: IOException) -> pure ()

mitfile :: Text -> FilePath
mitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)

-- Undo file utils

data Undo
  = Apply Text -- apply stash
  | Reset Text -- reset to commit
  | Revert Text -- revert commit
  deriving stock (Eq, Show)

showUndos :: [Undo] -> Text
showUndos =
  Text.intercalate " " . map showUndo
  where
    showUndo :: Undo -> Text
    showUndo = \case
      Apply commit -> "apply/" <> commit
      Reset commit -> "reset/" <> commit
      Revert commit -> "revert/" <> commit

parseUndos :: Text -> Maybe [Undo]
parseUndos t0 = do
  (Text.words >>> traverse parseUndo) t0
  where
    parseUndo :: Text -> Maybe Undo
    parseUndo text =
      asum
        [ Apply <$> Text.stripPrefix "apply/" text,
          Reset <$> Text.stripPrefix "reset/" text,
          Revert <$> Text.stripPrefix "revert/" text,
          error (show text)
        ]

applyUndos :: List1 Undo -> IO ()
applyUndos =
  traverse_ \case
    Apply commit -> void (gitApplyStash commit)
    Reset commit -> gitResetHard commit
    Revert commit -> git_ ["revert", commit]
