module Mit.Command.Commit
  ( mitCommit,
    -- FIXME move this
    abortIfCouldFastForwardToUpstream,
  )
where

import Mit.Git
  ( DiffResult (Differences, NoDifferences),
    GitCommitInfo (..),
    git,
    git2,
    gitApplyStash,
    gitCreateStash,
    gitCurrentBranch,
    gitDiff,
    gitExistCommitsBetween,
    gitFetch,
    gitIsMergeCommit,
    gitMaybeHead,
    gitMergeInProgress,
    gitNumCommitsBetween,
    gitNumCommitsOn,
    gitRemoteBranchHead,
    gitUnstageChanges,
  )
import Mit.Label (Label, goto)
import Mit.Logger (Logger, log)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo (..))
import Mit.Push
  ( DidntPushReason (NothingToPush, PushWouldBeRejected, PushWouldntReachRemote, TriedToPush),
    PushResult (DidntPush, Pushed),
    performPush,
    pushResultPushed,
  )
import Mit.Seq1 qualified as Seq1
import Mit.State (MitState (..), readMitState, writeMitState)
import Mit.Undo (Undo (..), undoStash)
import System.Exit (ExitCode (..))
import System.Posix.Terminal (queryTerminal)

mitCommit ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  (Maybe Undo -> IO ()) ->
  Text ->
  Bool ->
  Bool ->
  Maybe Text ->
  IO ()
mitCommit exit output pinfo sync gitdir allFlag dontSyncFlag maybeMessage = do
  gitMergeInProgress gitdir >>= \case
    False -> mitCommitNotMerge exit output pinfo gitdir allFlag dontSyncFlag maybeMessage
    True -> mitCommitMerge exit output pinfo sync gitdir dontSyncFlag

mitCommitMerge ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  (Maybe Undo -> IO ()) ->
  Text ->
  Bool ->
  IO ()
mitCommitMerge exit output pinfo sync gitdir dontSyncFlag = do
  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  head0 <- git pinfo ["rev-parse", "HEAD"]
  maybeState0 <- readMitState gitdir branch head0
  let maybeMerging = maybeState0 >>= \state0 -> state0.merging
  let maybeUndo = maybeState0 >>= \state0 -> state0.undo

  -- Make the merge commit. Commonly we'll have gotten here by `mit merge <branch>`, so we'll have a `state0.merging`
  -- that tells us we're merging in <branch>. But we also handle the case that we went `git merge` -> `mit commit`,
  -- because why not.
  case maybeMerging of
    Nothing -> git @() pinfo ["commit", "--all", "--no-edit"]
    Just source ->
      git @()
        pinfo
        [ "commit",
          "--all",
          "--message",
          fold ["⅄ ", if source == branch then "" else source <> " → ", branch]
        ]

  head1 <- git pinfo ["rev-parse", "HEAD"]

  -- Record that we are no longer merging.
  writeMitState
    gitdir
    branch
    MitState
      { head = head1,
        merging = Nothing,
        undo = maybeUndo
      }

  whenJust maybeMerging \source ->
    when (source /= branch) (log output (Output.MergeSucceeded Nothing))

  -- Three possible cases:
  --   1. We had a clean working directory before `mit merge`, so proceed to sync
  --   2. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers

  case maybeUndo >>= undoStash of
    Nothing -> when (not dontSyncFlag) (sync (Just (Reset head0)))
    Just stash -> do
      conflicts <- gitApplyStash pinfo stash
      case Seq1.fromList conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> when (not dontSyncFlag) (sync (Just (ResetApply head0 stash)))
        Just conflicts1 -> do
          log output (Output.UnstashFailed conflicts1)
          when (isJust maybeUndo) (log output Output.CanUndo)

mitCommitNotMerge ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  Text ->
  Bool ->
  Bool ->
  Maybe Text ->
  IO ()
mitCommitNotMerge exit output pinfo gitdir allFlag dontSyncFlag maybeMessage = do
  -- Check to see if there's even anything to commit, and bail if not.
  gitUnstageChanges pinfo
  gitDiff pinfo >>= \case
    Differences -> pure ()
    NoDifferences -> do
      log output Output.NothingToCommit
      goto exit (ExitFailure 1)

  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)

  fetched <-
    if dontSyncFlag
      then pure False
      else gitFetch pinfo "origin"
  maybeUpstreamHead <- gitRemoteBranchHead pinfo "origin" branch
  maybeHead0 <- gitMaybeHead pinfo
  abortIfCouldFastForwardToUpstream exit output pinfo maybeHead0 maybeUpstreamHead fetched

  maybeState0 <-
    case maybeHead0 of
      Nothing -> pure Nothing
      Just head0 -> readMitState gitdir branch head0
  maybeStash <- if isJust maybeHead0 then gitCreateStash pinfo else pure Nothing

  -- Initiate a commit, which (if interactive) can be cancelled with Ctrl+C.
  committed <- do
    doCommitAll <- if allFlag then pure True else not <$> queryTerminal 0
    case (doCommitAll, maybeMessage) of
      (True, Nothing) -> git2 pinfo ["commit", "--all", "--quiet"]
      (True, Just message) -> git pinfo ["commit", "--all", "--message", message, "--quiet"]
      (False, Nothing) -> git2 pinfo ["commit", "--patch", "--quiet"]
      (False, Just message) -> git2 pinfo ["commit", "--patch", "--message", message, "--quiet"]

  -- Get the new head after the commit (if successful)
  maybeHead1 <- if committed then Just <$> git pinfo ["rev-parse", "HEAD"] else pure maybeHead0

  -- Attempt a push (even if the commit was cancelled, since we might have other unpublished commits).
  pushResult <-
    if dontSyncFlag
      then pure (DidntPush NothingToPush)
      else performPush pinfo branch maybeHead1 maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  whenJust maybeHead1 \head1 -> do
    maybeRevert <-
      case pushResult of
        Pushed commits ->
          case Seq1.toList commits of
            [commit] ->
              gitIsMergeCommit pinfo commit.hash <&> \case
                False -> Just commit.hash
                True -> Nothing
            _ -> pure Nothing
        DidntPush _reason -> pure Nothing

    let maybeUndo1 =
          case (pushResultPushed pushResult, committed) of
            (False, False) -> maybeState0 >>= \state0 -> state0.undo
            (False, True) ->
              case maybeHead0 of
                Nothing -> Nothing
                Just head0 ->
                  Just case maybeStash of
                    Nothing -> Reset head0
                    Just stash -> ResetApply head0 stash
            (True, False) -> Revert <$> maybeRevert
            (True, True) -> do
              -- If we can revert the push *and* there is a stash in the snapshot (i.e. this *isnt* the very first
              -- commit), then we can undo (by reverting then applying the stash).
              --
              -- But if (for example) we can revert the push but there is *not* a stash in the snapshot, that means
              -- there were no commits before this one (`git stash create` is illegal there), so we don't want to
              -- offer to undo, because although we can revert the commit, we have no way of getting from there to
              -- back to having some dirty stuff to commit.
              RevertApply <$> maybeRevert <*> maybeStash

    writeMitState gitdir branch MitState {head = head1, merging = Nothing, undo = maybeUndo1}

    -- Whether we say we can undo from here is not exactly if the state says we can undo, because of one corner case: we
    -- ran 'mit commit', then aborted the commit, and ultimately didn't push any other local changes.
    --
    -- In this case, the underlying state hasn't changed, so 'mit undo' will still work as if the 'mit commit' was never
    -- run, we merely don't want to *say* "run 'mit undo' to undo" as feedback, because that sounds as if it would undo
    -- the last command run, namely the 'mit commit' that was aborted.
    when (isJust maybeUndo1 && committed) (log output Output.CanUndo)

-- If origin/branch is strictly ahead of branch (so we could fast-forward), abort, but if we successfully fetched,
-- because we do want to allow offline activity regardless.
abortIfCouldFastForwardToUpstream ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  Maybe Text ->
  Maybe Text ->
  Bool ->
  IO ()
abortIfCouldFastForwardToUpstream exit output pinfo maybeHead maybeUpstreamHead fetched = do
  when fetched do
    whenJust maybeUpstreamHead \upstreamHead -> do
      head <-
        maybeHead & onNothing do
          numRemoteCommits <- gitNumCommitsOn pinfo upstreamHead
          log output (Output.UpstreamIsAhead numRemoteCommits)
          goto exit (ExitFailure 1)
      numRemoteCommits <- gitNumCommitsBetween pinfo head upstreamHead
      when (numRemoteCommits > 0) do
        whenNotM (gitExistCommitsBetween pinfo upstreamHead head) do
          log output (Output.UpstreamIsAhead numRemoteCommits)
          goto exit (ExitFailure 1)
