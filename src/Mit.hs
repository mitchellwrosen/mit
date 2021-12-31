{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Mit where

import qualified Data.List.NonEmpty as List1
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Builder.ANSI as Text.Builder
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Mit.Git
import Mit.Prelude
import qualified Mit.Seq1 as Seq1
import qualified System.Clock as Clock
import System.Directory (doesDirectoryExist, removeFile, withCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)

-- FIXME: nicer "git status" story. in particular the conflict markers in the commits after a merge are a bit
-- ephemeral feeling
-- FIXME bail if active cherry-pick, active revert, active rebase, what else?
-- FIXME more Seq, less []

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
          gitl_ ["worktree", "add", "--detach", worktreeDir]
          withCurrentDirectory (Text.unpack worktreeDir) do
            gitBranchExists branch >>= \case
              False -> do
                gitBranch branch
                gitSwitch branch
                gitFetch_ "origin"
                whenM (gitRemoteBranchExists "origin" branch) do
                  let upstream = "origin/" <> branch
                  gitl_ ["reset", "--hard", upstream]
                  gitl_ ["branch", "--set-upstream-to", upstream]
              True -> gitSwitch branch
        True -> die ["Directory " <> Text.bold worktreeDir <> " already exists."]
    Just directory ->
      unless (directory == worktreeDir) do
        die [Text.bold branch <> " is already checked out in " <> Text.bold directory <> "."]
  where
    worktreeDir :: Text
    worktreeDir =
      Text.dropWhileEnd (/= '/') rootdir <> branch

mitCommit :: IO ()
mitCommit = do
  dieIfNotInGitDir
  whenM gitExistUntrackedFiles dieIfBuggyGit
  gitMergeInProgress >>= \case
    False ->
      gitDiff >>= \case
        Differences -> mitCommit_
        NoDifferences -> exitFailure
    True -> mitCommitMerge

mitCommit_ :: IO ()
mitCommit_ = do
  fetched <- gitFetch "origin"
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  head <- gitHead
  maybeUpstreamHead <- gitRemoteBranchHead "origin" branch
  existRemoteCommits <- maybe (pure False) (gitExistCommitsBetween head) maybeUpstreamHead
  existLocalCommits <- maybe (pure True) (\upstreamHead -> gitExistCommitsBetween upstreamHead "HEAD") maybeUpstreamHead
  state0 <- readMitState branch64

  let wouldFork = existRemoteCommits && not existLocalCommits
  let shouldWarnAboutFork =
        if wouldFork
          then do
            let theyRanMitCommitRecently =
                  case state0.ranCommitAt of
                    Nothing -> pure False
                    Just t0 -> do
                      t1 <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
                      pure ((t1 - t0) < 10_000_000_000)
            not <$> theyRanMitCommitRecently
          else pure False

  -- Bail out early if we should warn that this commit would fork history
  whenM shouldWarnAboutFork do
    ranCommitAt <- Just . Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
    writeMitState branch64 state0 {ranCommitAt}
    putLines
      [ "",
        "  " <> Text.yellow (Text.italic branch <> " is not up to date."),
        "",
        "  Run " <> Text.bold (Text.blue "mit sync") <> " first, or run " <> Text.bold (Text.blue "mit commit")
          <> " again to record a commit anyway.",
        ""
      ]
    exitFailure

  stash <- gitCreateStash
  committed <- gitCommit
  localCommits <- gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult <-
    case (localCommits, existRemoteCommits, fetched) of
      (Seq.Empty, _, _) -> pure (PushNotAttempted NothingToPush)
      (_ Seq.:<| _, True, _) -> pure (PushNotAttempted ForkedHistory)
      (_ Seq.:<| _, False, False) -> pure (PushNotAttempted Offline)
      (_ Seq.:<| _, False, True) -> PushAttempted <$> gitPush branch

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
      (False, False, _) -> pure state0.undos
      (False, True, _) -> pure [Reset head, Apply stash]
      (True, True, _ Seq.:<| Seq.Empty) -> do
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
          case Seq1.fromSeq localCommits of
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

mitCommitMerge :: IO ()
mitCommitMerge = do
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  head <- gitHead
  state0 <- readMitState branch64

  case state0.merging of
    Nothing -> gitl_ ["commit", "--all", "--no-edit"]
    Just merging ->
      let message = fold ["⅄ ", if merging == branch then "" else merging <> " → ", branch]
       in gitl_ ["commit", "--all", "--message", message]
  case listToMaybe [commit | Apply commit <- state0.undos] of
    Nothing -> mitSyncWith (Just [Reset head])
    Just stash ->
      gitApplyStash stash >>= \case
        -- FIXME we just unstashed, now we're about to stash again :/
        [] -> mitSyncWith (Just [Reset head, Apply stash])
        conflicts -> do
          writeMitState branch64 state0 {merging = Nothing, ranCommitAt = Nothing}
          putSummary
            Summary
              { branch,
                canUndo = not (null state0.undos),
                conflicts,
                syncs = []
              }

data PushResult
  = PushAttempted Bool
  | PushNotAttempted PushNotAttemptedReason

data PushNotAttemptedReason
  = ForkedHistory -- local history has forked, need to sync
  | NothingToPush -- no commits to push
  | Offline -- fetch failed, so we seem offline
  | UnseenCommits -- we just pulled remote commits; don't push in case there's something local to address

pushResultToSyncResult :: PushResult -> SyncResult
pushResultToSyncResult = \case
  PushAttempted False -> SyncResult'Failure
  PushAttempted True -> SyncResult'Success
  PushNotAttempted ForkedHistory -> SyncResult'Failure
  PushNotAttempted NothingToPush -> SyncResult'Success -- doesnt matter, wont be shown
  PushNotAttempted Offline -> SyncResult'Offline
  PushNotAttempted UnseenCommits -> SyncResult'Pending

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
                    MergeResult'MergeConflicts _ -> SyncResult'Failure
                    -- Even if we have conflicts from unstashing, we call this merge a success.
                    MergeResult'StashConflicts _ -> SyncResult'Success,
                source = target,
                target = branch
              }
      }

data MergeStatus = MergeStatus
  { commits :: Seq1 GitCommitInfo,
    result :: MergeResult,
    undos :: [Undo] -- FIXME List1
  }

data MergeResult
  = MergeResult'MergeConflicts (List1 GitConflict)
  | MergeResult'StashConflicts [GitConflict]

mitMerge' :: Text -> Text -> IO (Maybe MergeStatus)
mitMerge' message target = do
  head <- gitHead
  (Seq1.fromSeq <$> gitCommitsBetween (Just head) target) >>= \case
    Nothing -> pure Nothing
    Just commits -> do
      maybeStash <- gitStash
      let undos = Reset head : maybeToList (Apply <$> maybeStash)
      result <-
        gitl ["merge", "--ff", "--no-commit", target] >>= \case
          False -> do
            conflicts <- gitConflicts
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
            pure (MergeResult'MergeConflicts (List1.fromList conflicts))
          True -> do
            whenM gitMergeInProgress (gitl_ ["commit", "--message", message])
            maybeConflicts <- for maybeStash gitApplyStash
            pure (MergeResult'StashConflicts (fromMaybe [] maybeConflicts))
      pure (Just MergeStatus {commits, result, undos})

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: IO ()
mitSync = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit
  mitSyncWith Nothing

-- | @mitSyncWith maybeUndos@
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
mitSyncWith :: Maybe [Undo] -> IO ()
mitSyncWith maybeUndos = do
  fetched <- gitFetch "origin"
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  maybeUpstreamHead <- gitRemoteBranchHead "origin" branch

  maybeMergeStatus <-
    case maybeUpstreamHead of
      Nothing -> pure Nothing
      Just upstreamHead -> mitMerge' ("⅄ " <> branch) upstreamHead

  localCommits <- gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult <-
    case (localCommits, (.result) <$> maybeMergeStatus, fetched) of
      (Seq.Empty, _, _) -> pure (PushNotAttempted NothingToPush)
      -- "forked history" is ok - a bit different than history *already* having forked, in which case a push
      -- would just fail, whereas this is just us choosing not to push while in the middle of a merge due to a
      -- previous fork in the history
      (_ Seq.:<| _, Just (MergeResult'MergeConflicts _), _) -> pure (PushNotAttempted ForkedHistory)
      (_ Seq.:<| _, Just (MergeResult'StashConflicts _), _) -> pure (PushNotAttempted UnseenCommits)
      (_ Seq.:<| _, Nothing, False) -> pure (PushNotAttempted Offline)
      (_ Seq.:<| _, Nothing, True) -> PushAttempted <$> gitPush branch

  let pushed =
        case pushResult of
          PushAttempted success -> success
          PushNotAttempted _ -> False

  let undos =
        case pushed of
          False -> fromMaybe (maybe [] (.undos) maybeMergeStatus) maybeUndos
          True -> []

  writeMitState
    branch64
    MitState
      { head = (),
        merging = do
          mergeStatus <- maybeMergeStatus
          case mergeStatus.result of
            MergeResult'MergeConflicts _ -> Just branch
            MergeResult'StashConflicts _ -> Nothing,
        ranCommitAt = Nothing,
        undos
      }

  putSummary
    Summary
      { branch,
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
                          MergeResult'MergeConflicts _ -> SyncResult'Failure
                          MergeResult'StashConflicts _ -> SyncResult'Success,
                      source = "origin/" <> branch,
                      target = branch
                    },
              do
                commits <- Seq1.fromSeq localCommits
                pure
                  Sync
                    { commits,
                      result = pushResultToSyncResult pushResult,
                      source = branch,
                      target = "origin/" <> branch
                    }
            ]
      }

-- FIXME output what we just undid
mitUndo :: IO ()
mitUndo = do
  dieIfNotInGitDir

  branch64 <- Text.encodeBase64 <$> gitCurrentBranch
  state0 <- readMitState branch64
  case List1.nonEmpty state0.undos of
    Nothing -> exitFailure
    Just undos1 -> applyUndos undos1
  when (undosContainRevert state0.undos) mitSync
  where
    undosContainRevert :: [Undo] -> Bool
    undosContainRevert = \case
      [] -> False
      Revert _ : _ -> True
      _ : undos -> undosContainRevert undos

data Summary = Summary
  { branch :: Text,
    canUndo :: Bool,
    conflicts :: [GitConflict],
    syncs :: [Sync]
  }

data Sync = Sync
  { commits :: Seq1 GitCommitInfo,
    result :: SyncResult,
    source :: Text,
    target :: Text
  }

data SyncResult
  = SyncResult'Failure
  | SyncResult'Offline
  | SyncResult'Pending
  | SyncResult'Success

-- FIXME show some graph of where local/remote is at
putSummary :: Summary -> IO ()
putSummary summary =
  let output = concatMap syncLines summary.syncs ++ conflictsLines ++ undoLines
   in if null output then pure () else putLines (map (Text.Lazy.toStrict . Text.Builder.toLazyText) ("" : output))
  where
    conflictsLines :: [Text.Builder]
    conflictsLines =
      if null summary.conflicts
        then []
        else
          "  The following files have conflicts." :
          map (("    " <>) . Text.Builder.red . showGitConflict) summary.conflicts ++ [""]
    syncLines :: Sync -> [Text.Builder]
    syncLines sync =
      colorize
        ( Text.Builder.italic
            ("  " <> Text.Builder.fromText sync.source <> " → " <> Text.Builder.fromText sync.target)
        ) :
      map (("  " <>) . prettyGitCommitInfo) (toList @Seq commits')
        ++ (if more then ["  ...", ""] else [""])
      where
        colorize :: Text.Builder -> Text.Builder
        colorize =
          case sync.result of
            SyncResult'Failure -> Text.Builder.red
            SyncResult'Offline -> Text.Builder.brightBlack
            SyncResult'Pending -> Text.Builder.yellow
            SyncResult'Success -> Text.Builder.green
        (commits', more) =
          case Seq1.length sync.commits > 10 of
            False -> (Seq1.toSeq sync.commits, False)
            True -> (Seq1.dropEnd 1 sync.commits, True)
    undoLines :: [Text.Builder]
    undoLines =
      if summary.canUndo
        then ["  Run " <> Text.Builder.bold (Text.Builder.blue "mit undo") <> " to undo this change.", ""]
        else []

-- State file

data MitState a = MitState
  { head :: a,
    merging :: Maybe Text,
    ranCommitAt :: Maybe Integer,
    undos :: [Undo]
  }
  deriving stock (Eq, Show)

emptyMitState :: MitState ()
emptyMitState =
  MitState {head = (), merging = Nothing, ranCommitAt = Nothing, undos = []}

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

readMitState :: Text -> IO (MitState ())
readMitState branch64 = do
  head <- gitHead
  try (Text.readFile (mitfile branch64)) >>= \case
    Left (_ :: IOException) -> pure emptyMitState
    Right contents -> do
      let maybeState = do
            state <- parseMitState contents
            guard (head == state.head)
            pure state
      case maybeState of
        Nothing -> do
          deleteMitState branch64
          pure emptyMitState
        Just state -> pure (state {head = ()} :: MitState ())

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
    Revert commit -> gitRevert commit
