module Mit where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List1
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Builder.ANSI as Text.Builder
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Mit.Builder as Builder
import Mit.Clock (getCurrentTime)
import Mit.Directory
import Mit.Git
import Mit.Prelude
import qualified Mit.Seq as Seq
import qualified Mit.Seq1 as Seq1
import Mit.State
import Mit.Undo
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
  let validate (ver, err) = if version < ver then ((ver, err) :) else id
  case foldr validate [] validations of
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
    Nothing -> do
      whenM (doesDirectoryExist worktreeDir) (die ["Directory " <> Text.bold worktreeDir <> " already exists."])
      git_ ["worktree", "add", "--detach", worktreeDir]
      withCurrentDirectory worktreeDir do
        whenNotM (gitSwitch branch) do
          gitBranch branch
          gitSwitch_ branch
          gitFetch_ "origin"
          whenM (gitRemoteBranchExists "origin" branch) do
            let upstream = "origin/" <> branch
            git_ ["reset", "--hard", upstream]
            git_ ["branch", "--set-upstream-to", upstream]
    Just directory ->
      when (directory /= worktreeDir) do
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
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  head0 <- gitHead
  state0 <- readMitState branch64
  stash <- gitCreateStash
  let undos0 = [Reset head0, Apply stash]

  (fetched, maybeUpstreamHead, existRemoteCommits, wouldFork) <- do
    ago <- mitStateRanCommitAgo state0
    if ago < 10_000_000_000
      then do
        maybeUpstreamHead <- gitRemoteBranchHead "origin" branch
        pure (True, maybeUpstreamHead, True, True)
      else do
        fetched <- gitFetch "origin"
        maybeUpstreamHead <- gitRemoteBranchHead "origin" branch

        existRemoteCommits <- maybe (pure False) (gitExistCommitsBetween head0) maybeUpstreamHead
        existLocalCommits <-
          maybe
            (pure True)
            (\upstreamHead -> gitExistCommitsBetween upstreamHead head0)
            maybeUpstreamHead

        let wouldFork = existRemoteCommits && not existLocalCommits

        when wouldFork do
          applyUndo (Reset (fromJust maybeUpstreamHead))
          conflicts <- gitApplyStash stash
          for_ undos0 applyUndo

          ranCommitAt <- Just <$> getCurrentTime
          writeMitState branch64 state0 {ranCommitAt}

          Builder.putln
            if null conflicts
              then
                Builder.vcat
                  [ Builder.empty,
                    "  "
                      <> Text.Builder.yellow
                        ( Text.Builder.italic (Text.Builder.fromText branch)
                            <> " is not synchronized with "
                            <> Text.Builder.italic ("origin/" <> Text.Builder.fromText branch)
                            <> ", but would not become in conflict."
                        ),
                    Builder.empty,
                    "  To avoid a merge bubble, run " <> Text.Builder.bold (Text.Builder.blue "mit sync") <> " first.",
                    Builder.empty,
                    "  Otherwise, run "
                      <> Text.Builder.bold (Text.Builder.blue "mit commit")
                      <> " again (within 10 seconds) to record a commit anyway.",
                    Builder.empty
                  ]
              else
                Builder.vcat
                  [ Builder.empty,
                    Builder.vcat
                      ( "  "
                          <> Text.Builder.yellow
                            ( Text.Builder.italic (Text.Builder.fromText branch)
                                <> " would become in conflict with "
                                <> Text.Builder.italic ("origin/" <> Text.Builder.fromText branch)
                                <> "."
                            ) :
                        map (\conflict -> "    " <> Text.Builder.yellow (showGitConflict conflict)) conflicts
                      ),
                    Builder.empty,
                    "  To avoid a merge bubble, run "
                      <> Text.Builder.bold (Text.Builder.blue "mit sync")
                      <> " first to resolve conflicts.",
                    "  If conflicts seem too difficult to resolve now, you will be able to "
                      <> Text.Builder.bold (Text.Builder.blue "mit undo")
                      <> " to back out.",
                    Builder.empty,
                    "  Otherwise, run "
                      <> Text.Builder.bold (Text.Builder.blue "mit commit")
                      <> " again (within 10 seconds) to record a commit anyway.",
                    Builder.empty
                  ]
          exitFailure

        pure (fetched, maybeUpstreamHead, existRemoteCommits, wouldFork)

  committed <- gitCommit
  localCommits <- gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult <-
    case (localCommits, existRemoteCommits, fetched) of
      (Seq.Empty, _, _) -> pure (PushNotAttempted NothingToPush)
      (Seq.NonEmpty, True, _) -> pure (PushNotAttempted ForkedHistory)
      (Seq.NonEmpty, False, False) -> pure (PushNotAttempted Offline)
      (Seq.NonEmpty, False, True) -> PushAttempted <$> gitPush branch

  let pushed =
        case pushResult of
          PushAttempted success -> success
          PushNotAttempted _ -> False

  -- Only bother resetting the "ran commit at" if we would fork and the commit was aborted
  ranCommitAt <-
    case (wouldFork, committed) of
      (True, False) -> Just <$> getCurrentTime
      _ -> pure Nothing

  undos <-
    case (pushed, committed, localCommits) of
      (False, False, _) -> pure state0.undos
      (False, True, _) -> pure undos0
      (True, True, Seq.Singleton) -> do
        head1 <- gitHead
        pure [Revert head1, Apply stash]
      _ -> pure []

  writeMitState branch64 MitState {head = (), merging = Nothing, ranCommitAt, undos}

  putStanzas
    [ case Seq1.fromSeq localCommits of
        Nothing -> EmptyStanza
        Just commits ->
          SyncStanza
            Sync
              { commits,
                result = pushResultToSyncResult pushResult,
                source = branch,
                target = "origin/" <> branch
              },
      case pushResult of
        PushAttempted False -> RunSyncStanza
        PushAttempted True -> EmptyStanza
        PushNotAttempted ForkedHistory -> RunSyncStanza
        PushNotAttempted NothingToPush -> EmptyStanza
        PushNotAttempted Offline -> EmptyStanza
        PushNotAttempted UnseenCommits -> RunSyncStanza,
      -- Whether we say we can undo from here is not exactly if the state says we can undo, because of one corner case:
      -- we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
      --
      -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit' was
      -- never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as if it
      -- would undo the last command run, namely the 'mit commit' that was aborted.
      if not (null undos) && committed then CanUndoStanza else EmptyStanza
    ]

mitCommitMerge :: IO ()
mitCommitMerge = do
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  head <- gitHead
  state0 <- readMitState branch64

  case state0.merging of
    Nothing -> git_ ["commit", "--all", "--no-edit"]
    Just merging ->
      let message = fold ["⅄ ", if merging == branch then "" else merging <> " → ", branch]
       in git_ ["commit", "--all", "--message", message]

  case listToMaybe [commit | Apply commit <- state0.undos] of
    Nothing -> mitSyncWith (Just [Reset head])
    Just stash -> do
      conflicts <- gitApplyStash stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> mitSyncWith (Just [Reset head, Apply stash])
        Just conflicts1 -> do
          writeMitState branch64 state0 {merging = Nothing, ranCommitAt = Nothing}
          putStanzas
            [ ConflictsStanza conflicts1,
              if null state0.undos then EmptyStanza else CanUndoStanza
            ]

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
  PushAttempted False -> SyncResult'Failure SyncFailureReason'PushFailed
  PushAttempted True -> SyncResult'Success
  PushNotAttempted ForkedHistory -> SyncResult'Failure SyncFailureReason'ForkedHistory
  PushNotAttempted NothingToPush -> SyncResult'Success -- doesnt matter, wont be shown
  PushNotAttempted Offline -> SyncResult'Failure SyncFailureReason'Offline
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
            Just mergeStatus -> List1.toList mergeStatus.undos
      }

  putStanzas
    [ case maybeMergeStatus of
        Nothing -> EmptyStanza
        Just mergeStatus ->
          SyncStanza
            Sync
              { commits = mergeStatus.commits,
                result =
                  case mergeStatus.result of
                    MergeResult'MergeConflicts _ -> SyncResult'Failure SyncFailureReason'MergeConflicts
                    -- Even if we have conflicts from unstashing, we call this merge a success.
                    MergeResult'StashConflicts _ -> SyncResult'Success,
                source = target,
                target = branch
              },
      fromMaybe EmptyStanza do
        mergeStatus <- maybeMergeStatus
        case mergeStatus.result of
          MergeResult'MergeConflicts conflicts -> Just (ConflictsStanza conflicts)
          MergeResult'StashConflicts conflicts -> ConflictsStanza <$> List1.nonEmpty conflicts,
      if isJust maybeMergeStatus then CanUndoStanza else EmptyStanza
    ]

data MergeStatus = MergeStatus
  { commits :: Seq1 GitCommitInfo,
    result :: MergeResult,
    undos :: List1 Undo
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
      let undos = Reset head :| maybeToList (Apply <$> maybeStash)
      result <-
        git ["merge", "--ff", "--no-commit", target] >>= \case
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
      (Seq.NonEmpty, Just (MergeResult'MergeConflicts _), _) -> pure (PushNotAttempted ForkedHistory)
      (Seq.NonEmpty, Just (MergeResult'StashConflicts _), _) -> pure (PushNotAttempted UnseenCommits)
      (Seq.NonEmpty, Nothing, False) -> pure (PushNotAttempted Offline)
      (Seq.NonEmpty, Nothing, True) -> PushAttempted <$> gitPush branch

  let pushed =
        case pushResult of
          PushAttempted success -> success
          PushNotAttempted _ -> False

  let undos =
        case pushed of
          False -> fromMaybe (maybe [] (List1.toList . (.undos)) maybeMergeStatus) maybeUndos
          True -> []

  writeMitState
    (Text.encodeBase64 branch)
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

  putStanzas
    [ case maybeMergeStatus of
        Nothing -> EmptyStanza
        Just mergeStatus ->
          SyncStanza
            Sync
              { commits = mergeStatus.commits,
                result =
                  case mergeStatus.result of
                    MergeResult'MergeConflicts _ -> SyncResult'Failure SyncFailureReason'MergeConflicts
                    MergeResult'StashConflicts _ -> SyncResult'Success,
                source = "origin/" <> branch,
                target = branch
              },
      case Seq1.fromSeq localCommits of
        Nothing -> EmptyStanza
        Just commits ->
          SyncStanza
            Sync
              { commits,
                result = pushResultToSyncResult pushResult,
                source = branch,
                target = "origin/" <> branch
              },
      fromMaybe EmptyStanza do
        mergeStatus <- maybeMergeStatus
        case mergeStatus.result of
          MergeResult'MergeConflicts conflicts -> Just (ConflictsStanza conflicts)
          MergeResult'StashConflicts conflicts -> ConflictsStanza <$> List1.nonEmpty conflicts,
      case pushResult of
        PushAttempted False -> RunSyncStanza
        PushAttempted True -> EmptyStanza
        PushNotAttempted ForkedHistory -> RunSyncStanza
        PushNotAttempted NothingToPush -> EmptyStanza
        PushNotAttempted Offline -> EmptyStanza
        PushNotAttempted UnseenCommits -> RunSyncStanza,
      if not (null undos) then CanUndoStanza else EmptyStanza
    ]

-- FIXME output what we just undid
mitUndo :: IO ()
mitUndo = do
  dieIfNotInGitDir

  branch64 <- Text.encodeBase64 <$> gitCurrentBranch
  state0 <- readMitState branch64
  case List1.nonEmpty state0.undos of
    Nothing -> exitFailure
    Just undos1 -> for_ undos1 applyUndo
  when (undosContainRevert state0.undos) mitSync
  where
    undosContainRevert :: [Undo] -> Bool
    undosContainRevert = \case
      [] -> False
      Revert _ : _ -> True
      _ : undos -> undosContainRevert undos

data Stanza
  = CanUndoStanza
  | ConflictsStanza (List1 GitConflict)
  | EmptyStanza
  | RunSyncStanza
  | SyncStanza Sync

data Sync = Sync
  { commits :: Seq1 GitCommitInfo,
    result :: SyncResult,
    source :: Text,
    target :: Text
  }

data SyncFailureReason
  = SyncFailureReason'ForkedHistory
  | SyncFailureReason'MergeConflicts
  | SyncFailureReason'PushFailed
  | SyncFailureReason'Offline
  deriving stock (Eq)

data SyncResult
  = SyncResult'Failure SyncFailureReason
  | SyncResult'Pending
  | SyncResult'Success
  deriving stock (Eq)

renderStanza :: Stanza -> Text.Builder
renderStanza = \case
  CanUndoStanza -> "  Run " <> Text.Builder.bold (Text.Builder.blue "mit undo") <> " to undo this change."
  ConflictsStanza conflicts ->
    "  The following files are in conflict.\n"
      <> Builder.vcat ((\conflict -> "    " <> Text.Builder.red (showGitConflict conflict)) <$> conflicts)
  EmptyStanza -> mempty
  RunSyncStanza ->
    "  Run " <> Text.Builder.bold (Text.Builder.blue "mit sync")
      <> " to synchronize with "
      <> Text.Builder.italic "origin"
      <> "."
  SyncStanza sync ->
    colorize
      (Text.Builder.italic ("  " <> Text.Builder.fromText sync.source <> " → " <> Text.Builder.fromText sync.target))
      <> "\n"
      <> (Builder.vcat ((\commit -> "  " <> prettyGitCommitInfo commit) <$> commits'))
      <> (if more then "  ..." else Builder.empty)
    where
      colorize :: Text.Builder -> Text.Builder
      colorize =
        case sync.result of
          SyncResult'Failure SyncFailureReason'ForkedHistory -> Text.Builder.red
          SyncResult'Failure SyncFailureReason'MergeConflicts -> Text.Builder.red
          SyncResult'Failure SyncFailureReason'PushFailed -> Text.Builder.red
          SyncResult'Failure SyncFailureReason'Offline -> Text.Builder.brightBlack
          SyncResult'Pending -> Text.Builder.yellow
          SyncResult'Success -> Text.Builder.green
      (commits', more) =
        case Seq1.length sync.commits > 10 of
          False -> (Seq1.toSeq sync.commits, False)
          True -> (Seq1.dropEnd 1 sync.commits, True)

putStanzas :: [Stanza] -> IO ()
putStanzas stanzas =
  if s == "\n" then pure () else Text.putStr s
  where
    s = Builder.build (Builder.newline <> foldr f Builder.empty stanzas)
    f = \case
      EmptyStanza -> id
      stanza -> \acc -> renderStanza stanza <> "\n\n" <> acc
