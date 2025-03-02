module Mit.Command.Merge
  ( mitMerge,
  )
where

import Mit.Command.Commit (abortIfCouldFastForwardToUpstream)
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
import System.Exit (ExitCode (..))
import UnconditionalJump (Label, goto)

mitMerge :: Label ExitCode -> Logger Output -> IO () -> Text -> Text -> IO ()
mitMerge exit output sync gitdir source = do
  let pinfo = Output.ProcessInfo >$< output

  whenM (gitMergeInProgress gitdir) do
    log output Output.MergeInProgress
    goto exit (ExitFailure 1)

  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)

  let upstream = "origin/" <> branch

  -- Special case: if on branch "foo", treat `mit merge foo` and `mit merge origin/foo` as `mit sync`
  case source == branch || source == upstream of
    True -> sync
    False -> mitMerge_ exit output gitdir source branch

mitMerge_ :: Label ExitCode -> Logger Output -> Text -> Text -> Text -> IO ()
mitMerge_ exit output gitdir source branch = do
  let pinfo = Output.ProcessInfo >$< output

  fetched <- gitFetch pinfo "origin"
  maybeHead0 <- gitMaybeHead pinfo
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  snapshot <- performSnapshot pinfo

  -- When given 'mit merge foo', prefer running 'git merge origin/foo' over 'git merge foo'
  sourceCommit <-
    gitRemoteBranchHead pinfo "origin" source & onNothingM do
      git pinfo ["rev-parse", "refs/heads/" <> source] & onLeftM \_ -> do
        log output Output.NoSuchBranch
        goto exit (ExitFailure 1)

  abortIfCouldFastForwardToUpstream exit output maybeHead0 maybeUpstreamHead fetched

  whenJust (snapshotStash snapshot) \_stash ->
    git @() pinfo ["reset", "--hard", "--quiet", "HEAD"]

  mergeResult <- performMerge pinfo gitdir sourceCommit ("⅄ " <> source <> " → " <> branch)

  log output case mergeResult of
    NothingToMerge -> Output.NothingToMerge source branch
    TriedToMerge commits conflicts -> Output.MergeFailed commits conflicts
    Merged commits -> Output.MergeSucceeded (Just commits)

  head1 <- git pinfo ["rev-parse", "HEAD"] -- FIXME oops this can be Nothing
  pushResult <- performPush pinfo branch (Just head1) maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  let undo1 =
        if pushResultPushed pushResult || isNothing (mergeResultConflicts mergeResult)
          then Nothing
          else undoToSnapshot snapshot

  writeMitState
    gitdir
    branch
    MitState
      { head = head1,
        merging =
          case mergeResultConflicts mergeResult of
            Nothing -> Nothing
            Just _conflicts -> Just source,
        undo = undo1
      }

  when (isNothing (mergeResultConflicts mergeResult)) do
    whenJust (snapshotStash snapshot) \stash -> do
      conflicts0 <- gitApplyStash pinfo stash
      whenJust (Seq1.fromList conflicts0) \conflicts1 -> log output (Output.UnstashFailed conflicts1)

  when (isJust undo1) (log output Output.CanUndo)
