module Mit.Command.Sync
  ( mitSync,
    mitSyncWith,
  )
where

import Mit.Git
  ( git,
    gitApplyStash,
    gitCurrentBranch,
    gitFetch,
    gitMaybeHead,
    gitMergeInProgress,
    gitRemoteBranchHead,
  )
import Mit.Logger (Logger, log)
import Mit.Merge (MergeResult (..), mergeResultConflicts, performMerge)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.Push
  ( DidntPushReason (NothingToPush, PushWouldBeRejected, PushWouldntReachRemote, TriedToPush),
    PushResult (DidntPush, Pushed),
    performPush,
    pushResultPushed,
  )
import Mit.Seq1 qualified as Seq1
import Mit.Snapshot (performSnapshot, snapshotStash, undoToSnapshot)
import Mit.State (MitState (..), writeMitState)
import Mit.Undo (Undo (..))
import System.Exit (ExitCode (..))
import UnconditionalJump (Label, goto)

-- TODO implement "lateral sync", i.e. a merge from some local or remote branch, followed by a sync to upstream
mitSync :: Label ExitCode -> Logger Output -> Text -> IO ()
mitSync exit output gitdir = do
  whenM (gitMergeInProgress gitdir) do
    log output Output.MergeInProgress
    goto exit (ExitFailure 1)
  mitSyncWith exit output gitdir Nothing

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
mitSyncWith :: Label ExitCode -> Logger Output -> Text -> Maybe Undo -> IO ()
mitSyncWith exit output gitdir maybeUndo = do
  let pinfo = Output.ProcessInfo >$< output

  fetched <- gitFetch pinfo "origin"
  branch <- do
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  snapshot <- performSnapshot pinfo

  whenJust (snapshotStash snapshot) \_stash ->
    git @() pinfo ["reset", "--hard", "--quiet", "HEAD"]

  mergeResult <-
    case maybeUpstreamHead of
      -- Yay: no upstream branch is not different from an up-to-date local branch
      Nothing -> pure NothingToMerge
      Just upstreamHead -> performMerge pinfo gitdir upstreamHead ("⅄ " <> branch)

  case mergeResult of
    NothingToMerge -> pure ()
    TriedToMerge commits conflicts -> log output (Output.PullFailed commits conflicts)
    Merged commits -> log output (Output.PullSucceeded commits)

  maybeHead1 <- gitMaybeHead pinfo

  pushResult <- performPush pinfo branch maybeHead1 maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  let undo1 =
        case (pushResultPushed pushResult, maybeUndo, mergeResultConflicts mergeResult) of
          (True, _, _) -> Nothing
          -- FIXME hm, could consider appending those undos instead, even if they obviate the recent stash/merge
          -- undos
          (False, Just undo, _) -> Just undo
          (False, Nothing, Nothing) -> Nothing
          (False, Nothing, Just _conflicts) -> undoToSnapshot snapshot

  whenJust maybeHead1 \head1 ->
    writeMitState
      gitdir
      branch
      MitState
        { head = head1,
          merging =
            case mergeResultConflicts mergeResult of
              Nothing -> Nothing
              Just _conflicts -> Just branch,
          undo = undo1
        }

  when (isNothing (mergeResultConflicts mergeResult)) do
    whenJust (snapshotStash snapshot) \stash -> do
      conflicts0 <- gitApplyStash pinfo stash
      whenJust (Seq1.fromList conflicts0) \conflicts1 -> log output (Output.UnstashFailed conflicts1)

  when (isJust undo1) (log output Output.CanUndo)
