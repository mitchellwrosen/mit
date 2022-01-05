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
import Mit.Stanza
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
-- TODO undo revert
-- TODO more specific "undo this change" wording

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
  let upstream = "origin/" <> branch
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

          putStanzas $
            case List1.nonEmpty conflicts of
              Nothing ->
                [ notSynchronizedStanza
                    (Text.Builder.fromText branch)
                    (Text.Builder.fromText upstream)
                    ", but committing these changes would not put it in conflict.",
                  Just
                    ( "  To avoid making a merge commit, run "
                        <> Text.Builder.bold (Text.Builder.blue "mit sync")
                        <> " first."
                    ),
                  Just
                    ( "  Otherwise, run "
                        <> Text.Builder.bold (Text.Builder.blue "mit commit")
                        <> " again (within 10 seconds) to record a commit anyway."
                    )
                ]
              Just conflicts1 ->
                [ notSynchronizedStanza
                    (Text.Builder.fromText branch)
                    (Text.Builder.fromText upstream)
                    ", and committing these changes would put it in conflict.",
                  conflictsStanza "These files would be in conflict:" conflicts1,
                  Just
                    ( Builder.vcat
                        [ "  To avoid making a merge commit, run "
                            <> Text.Builder.bold (Text.Builder.blue "mit sync")
                            <> " first to resolve conflicts.",
                          "  (If conflicts seem too difficult to resolve, you will be able to "
                            <> Text.Builder.bold (Text.Builder.blue "mit undo")
                            <> " to back out)."
                        ]
                    ),
                  Just
                    ( "  Otherwise, run "
                        <> Text.Builder.bold (Text.Builder.blue "mit commit")
                        <> " again (within 10 seconds) to record a commit anyway."
                    )
                ]
          exitFailure

        pure (fetched, maybeUpstreamHead, existRemoteCommits, wouldFork)

  committed <- gitCommit
  head1 <- if committed then gitHead else pure head0
  localCommits <- gitCommitsBetween maybeUpstreamHead head1

  pushResult <-
    case (localCommits, existRemoteCommits, fetched) of
      (Seq.Empty, _, _) -> pure (PushNotAttempted NothingToPush)
      (Seq.NonEmpty, True, _) -> do
        conflicts <- gitConflictsWith (fromJust maybeUpstreamHead)
        pure (PushNotAttempted (ForkedHistory conflicts))
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
      (True, True, Seq.Singleton) -> pure [Revert head1, Apply stash]
      _ -> pure []

  writeMitState branch64 MitState {head = (), merging = Nothing, ranCommitAt, undos}

  remoteCommits <-
    if existRemoteCommits
      then gitCommitsBetween (Just head1) (fromJust maybeUpstreamHead)
      else pure Seq.empty

  putStanzas
    [ isSynchronizedStanza (Text.Builder.fromText branch) pushResult,
      do
        commits <- Seq1.fromSeq remoteCommits
        syncStanza
          Sync
            { commits,
              result = SyncResult'Failure,
              source = upstream,
              target = branch
            },
      do
        commits <- Seq1.fromSeq localCommits
        syncStanza
          Sync
            { commits,
              result = pushResultToSyncResult pushResult,
              source = branch,
              target = upstream
            },
      case pushResult of
        PushNotAttempted (ForkedHistory (List1.nonEmpty -> Just conflicts)) ->
          conflictsStanza
            ("These files will be in conflict when you run " <> Text.Builder.bold (Text.Builder.blue "mit sync") <> ":")
            conflicts
        _ -> Nothing,
      whatNextStanza (Text.Builder.fromText branch) pushResult,
      -- Whether we say we can undo from here is not exactly if the state says we can undo, because of one corner case:
      -- we ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
      --
      -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit' was
      -- never run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as if it
      -- would undo the last command run, namely the 'mit commit' that was aborted.
      if not (null undos) && committed then canUndoStanza else Nothing
    ]

mitCommitMerge :: IO ()
mitCommitMerge = do
  branch <- gitCurrentBranch
  let branch64 = Text.encodeBase64 branch
  head <- gitHead
  state0 <- readMitState branch64

  -- Make the merge commit. Commonly we'll have gotten here by `mit merge <branch>`, so we'll have a `state0.merging`
  -- that tells us we're merging in <branch>. But we also handle the case that we went `git merge` -> `mit commit`,
  -- because why not.
  case state0.merging of
    Nothing -> git_ ["commit", "--all", "--no-edit"]
    Just merging ->
      let message = fold ["⅄ ", if merging == branch then "" else merging <> " → ", branch]
       in git_ ["commit", "--all", "--message", message]

  writeMitState branch64 state0 {merging = Nothing, ranCommitAt = Nothing}

  let stanza0 = do
        merging <- state0.merging
        guard (merging /= branch)
        synchronizedStanza (Text.Builder.fromText branch) (Text.Builder.fromText merging)

  -- Three possible cases:
  --   1. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers
  --   2. We had a clean working directory before `mit merge`, so proceed to sync

  case undosStash state0.undos of
    Nothing -> mitSyncWith stanza0 (Just [Reset head])
    Just stash -> do
      conflicts <- gitApplyStash stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> mitSyncWith stanza0 (Just [Reset head, Apply stash])
        Just conflicts1 ->
          putStanzas
            [ stanza0,
              conflictsStanza "These files are in conflict:" conflicts1,
              -- Fake like we didn't push due to merge conflicts just to print "resolve conflicts and commit"
              whatNextStanza (Text.Builder.fromText branch) (PushNotAttempted MergeConflicts),
              if null state0.undos then Nothing else canUndoStanza
            ]

data PushResult
  = PushAttempted Bool
  | PushNotAttempted PushNotAttemptedReason

data PushNotAttemptedReason
  = ForkedHistory [GitConflict] -- local history has forked, need to sync.
  | MergeConflicts -- local merge conflicts that need to be resolved right now
  | NothingToPush -- no commits to push
  | Offline -- fetch failed, so we seem offline
  | UnseenCommits -- we just pulled remote commits; don't push in case there's something local to address

pushResultToSyncResult :: PushResult -> SyncResult
pushResultToSyncResult = \case
  PushAttempted False -> SyncResult'Failure
  PushAttempted True -> SyncResult'Success
  PushNotAttempted (ForkedHistory _) -> SyncResult'Failure
  PushNotAttempted MergeConflicts -> SyncResult'Failure
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

  {- new -}

  (mergedCommits, mergeConflicts) <- performMerge ("⅄ " <> target <> " → " <> branch) targetCommit

  {- end new -}

  maybeMergeStatus <- mitMerge' ("⅄ " <> target <> " → " <> branch) targetCommit

  let maybeMergeResult = (.result) <$> maybeMergeStatus

  let conflicts =
        case maybeMergeResult of
          Nothing -> []
          Just (MergeResult'MergeConflicts conflicts1) -> List1.toList conflicts1
          Just (MergeResult'StashConflicts conflicts1) -> List1.toList conflicts1
          Just MergeResult'Success -> []

  writeMitState
    branch64
    MitState
      { head = (),
        merging = do
          result <- maybeMergeResult
          if mergeResultCommitted result
            then Nothing
            else Just target,
        ranCommitAt = Nothing,
        undos =
          case maybeMergeStatus of
            Nothing -> []
            Just mergeStatus -> List1.toList mergeStatus.undos
      }

  let branchb = Text.Builder.fromText branch
  let targetb = Text.Builder.fromText target

  putStanzas
    [ case maybeMergeResult of
        Nothing -> synchronizedStanza branchb targetb
        Just result ->
          if mergeResultCommitted result
            then synchronizedStanza branchb targetb
            else notSynchronizedStanza branchb targetb ".",
      do
        mergeStatus <- maybeMergeStatus
        syncStanza
          Sync
            { commits = mergeStatus.commits,
              result = if mergeResultCommitted mergeStatus.result then SyncResult'Success else SyncResult'Failure,
              source = target,
              target = branch
            },
      do
        result <- maybeMergeResult
        if mergeResultCommitted result
          then isSynchronizedStanza branchb (PushNotAttempted UnseenCommits)
          else Nothing,
      do
        conflicts1 <- List1.nonEmpty conflicts
        conflictsStanza "These files are in conflict:" conflicts1,
      do
        result <- maybeMergeResult
        whatNextStanza
          branchb
          ( case result of
              MergeResult'MergeConflicts _ -> PushNotAttempted MergeConflicts
              MergeResult'StashConflicts _ -> PushNotAttempted UnseenCommits
              MergeResult'Success -> PushNotAttempted UnseenCommits
          ),
      if isJust maybeMergeStatus then canUndoStanza else Nothing
    ]

data MergeStatus = MergeStatus
  { commits :: Seq1 GitCommitInfo,
    result :: MergeResult,
    undos :: List1 Undo
  }

data MergeResult
  = MergeResult'MergeConflicts (List1 GitConflict)
  | MergeResult'StashConflicts (List1 GitConflict)
  | MergeResult'Success

-- | Did this merge commit (possibly leaving behind a conflicting unstash)?
mergeResultCommitted :: MergeResult -> Bool
mergeResultCommitted = \case
  MergeResult'MergeConflicts _ -> False
  MergeResult'StashConflicts _ -> True
  MergeResult'Success -> True

mitMerge' :: Text -> Text -> IO (Maybe MergeStatus)
mitMerge' message target = do
  head <- gitHead
  maybeStash <- gitStash
  (commits, mergeConflicts) <- performMerge message target
  if Seq.null commits
    then do
      for_ maybeStash gitApplyStash
      pure Nothing
    else do
      let undos = Reset head :| maybeToList (Apply <$> maybeStash)
      result <-
        case List1.nonEmpty mergeConflicts of
          Nothing -> do
            stashConflicts <-
              case maybeStash of
                Nothing -> pure []
                Just stash -> gitApplyStash stash
            pure case List1.nonEmpty stashConflicts of
              Nothing -> MergeResult'Success
              Just stashConflicts1 -> (MergeResult'StashConflicts stashConflicts1)
          Just mergeConflicts1 -> pure (MergeResult'MergeConflicts mergeConflicts1)
      pure (Just MergeStatus {commits = Seq1.unsafeFromSeq commits, result, undos})

-- TODO document precondition clean working tree
-- invariant: can't return conflicts but no commits
performMerge :: Text -> Text -> IO (Seq GitCommitInfo, [GitConflict])
performMerge message commitish = do
  head <- gitHead
  commits <- gitCommitsBetween (Just head) commitish
  conflicts <-
    if Seq.null commits
      then pure []
      else do
        git ["merge", "--ff", "--no-commit", commitish] >>= \case
          False -> gitConflicts
          True -> do
            -- If this was a fast-forward, a merge would not be in progress at this point.
            whenM gitMergeInProgress (git_ ["commit", "--message", message])
            pure []
  pure (commits, conflicts)

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: IO ()
mitSync = do
  dieIfNotInGitDir
  dieIfMergeInProgress
  whenM gitExistUntrackedFiles dieIfBuggyGit
  mitSyncWith Nothing Nothing

-- | @mitSyncWith _ maybeUndos@
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
mitSyncWith :: Maybe Text.Builder -> Maybe [Undo] -> IO ()
mitSyncWith stanza0 maybeUndos = do
  fetched <- gitFetch "origin"
  branch <- gitCurrentBranch
  let upstream = "origin/" <> branch
  maybeUpstreamHead <- gitRemoteBranchHead "origin" branch

  maybeMergeStatus <-
    case maybeUpstreamHead of
      Nothing -> pure Nothing
      Just upstreamHead -> mitMerge' ("⅄ " <> branch) upstreamHead

  let maybeMergeResult = (.result) <$> maybeMergeStatus

  let conflicts =
        case maybeMergeResult of
          Nothing -> []
          Just (MergeResult'MergeConflicts conflicts1) -> List1.toList conflicts1
          Just (MergeResult'StashConflicts conflicts1) -> List1.toList conflicts1
          Just MergeResult'Success -> []

  localCommits <- gitCommitsBetween maybeUpstreamHead "HEAD"

  pushResult <-
    case (localCommits, maybeMergeResult, fetched) of
      (Seq.Empty, _, _) -> pure (PushNotAttempted NothingToPush)
      (Seq.NonEmpty, Just (MergeResult'MergeConflicts _), _) -> pure (PushNotAttempted MergeConflicts)
      (Seq.NonEmpty, Just (MergeResult'StashConflicts _), _) -> pure (PushNotAttempted UnseenCommits)
      (Seq.NonEmpty, Just MergeResult'Success, _) -> pure (PushNotAttempted UnseenCommits)
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
          result <- maybeMergeResult
          if mergeResultCommitted result
            then Nothing
            else Just branch,
        ranCommitAt = Nothing,
        undos
      }

  putStanzas
    [ stanza0,
      isSynchronizedStanza (Text.Builder.fromText branch) pushResult,
      do
        mergeStatus <- maybeMergeStatus
        syncStanza
          Sync
            { commits = mergeStatus.commits,
              result = if mergeResultCommitted mergeStatus.result then SyncResult'Success else SyncResult'Failure,
              source = upstream,
              target = branch
            },
      do
        commits <- Seq1.fromSeq localCommits
        syncStanza
          Sync
            { commits,
              result = pushResultToSyncResult pushResult,
              source = branch,
              target = upstream
            },
      do
        conflicts1 <- List1.nonEmpty conflicts
        conflictsStanza "These files are in conflict:" conflicts1,
      whatNextStanza (Text.Builder.fromText branch) pushResult,
      if not (null undos) then canUndoStanza else Nothing
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
  deriving stock (Eq)

canUndoStanza :: Stanza
canUndoStanza =
  Just ("  Run " <> Text.Builder.bold (Text.Builder.blue "mit undo") <> " to undo this change.")

conflictsStanza :: Text.Builder -> List1 GitConflict -> Stanza
conflictsStanza prefix conflicts =
  Just $
    "  "
      <> prefix
      <> Builder.newline
      <> Builder.vcat ((\conflict -> "    " <> Text.Builder.red (showGitConflict conflict)) <$> conflicts)

isSynchronizedStanza :: Text.Builder -> PushResult -> Stanza
isSynchronizedStanza branch = \case
  PushAttempted False ->
    notSynchronizedStanza branch upstream (" because " <> Text.Builder.bold "git push" <> " failed.")
  PushAttempted True -> synchronizedStanza branch upstream
  PushNotAttempted MergeConflicts ->
    notSynchronizedStanza branch upstream " because you have local conflicts to resolve."
  PushNotAttempted (ForkedHistory _) -> notSynchronizedStanza branch upstream "; their commit histories have diverged."
  PushNotAttempted NothingToPush -> synchronizedStanza branch upstream
  PushNotAttempted Offline -> notSynchronizedStanza branch upstream " because you appear to be offline."
  PushNotAttempted UnseenCommits -> notSynchronizedStanza branch upstream "."
  where
    upstream = "origin/" <> branch

notSynchronizedStanza :: Text.Builder -> Text.Builder -> Text.Builder -> Stanza
notSynchronizedStanza branch other suffix =
  Just
    ( "  "
        <> Text.Builder.red
          ( Text.Builder.italic branch
              <> " is not synchronized with "
              <> Text.Builder.italic other
              <> suffix
          )
    )

whatNextStanza :: Text.Builder -> PushResult -> Stanza
whatNextStanza branch = \case
  PushAttempted False ->
    Just $
      "  Run "
        <> sync
        <> " to synchronize "
        <> Text.Builder.italic branch
        <> " with "
        <> Text.Builder.italic upstream
        <> "."
  PushAttempted True -> Nothing
  PushNotAttempted (ForkedHistory conflicts) ->
    Just $
      if null conflicts
        then
          "  Run "
            <> sync
            <> ", examine the repository, then run "
            <> sync
            <> " again to synchronize "
            <> Text.Builder.italic branch
            <> " with "
            <> Text.Builder.italic upstream
            <> "."
        else
          "  Run "
            <> sync
            <> ", resolve the conflicts, then run "
            <> commit
            <> " to synchronize "
            <> Text.Builder.italic branch
            <> " with "
            <> Text.Builder.italic upstream
            <> "."
  PushNotAttempted MergeConflicts ->
    Just ("  Resolve the merge conflicts, then run " <> commit <> ".")
  PushNotAttempted NothingToPush -> Nothing
  PushNotAttempted Offline ->
    Just $
      "  When you come online, run "
        <> sync
        <> " to synchronize "
        <> Text.Builder.italic branch
        <> " with "
        <> Text.Builder.italic upstream
        <> "."
  PushNotAttempted UnseenCommits ->
    Just $
      "  Examine the repository, then run "
        <> sync
        <> " to synchronize "
        <> Text.Builder.italic branch
        <> " with "
        <> Text.Builder.italic upstream
        <> "."
  where
    commit = Text.Builder.bold (Text.Builder.blue "mit commit")
    sync = Text.Builder.bold (Text.Builder.blue "mit sync")
    upstream = "origin/" <> branch

syncStanza :: Sync -> Stanza
syncStanza sync =
  Just $
    Text.Builder.italic
      (colorize ("    " <> Text.Builder.fromText sync.source <> " → " <> Text.Builder.fromText sync.target))
      <> "\n"
      <> (Builder.vcat ((\commit -> "    " <> prettyGitCommitInfo commit) <$> commits'))
      <> (if more then "    ..." else Builder.empty)
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

synchronizedStanza :: Text.Builder -> Text.Builder -> Stanza
synchronizedStanza branch other =
  Just
    ( "  "
        <> Text.Builder.green
          (Text.Builder.italic branch <> " is synchronized with " <> Text.Builder.italic other <> ".")
    )
