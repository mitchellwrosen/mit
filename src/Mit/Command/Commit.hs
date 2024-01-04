module Mit.Command.Commit
  ( mitCommit,
    -- FIXME move this
    abortIfCouldFastForwardToUpstream,
  )
where

import Data.List.NonEmpty qualified as List1
import Mit.Git
  ( DiffResult (Differences, NoDifferences),
    GitCommitInfo (..),
    GitConflict (..),
    GitConflictXY (..),
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
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Mit.ProcessInfo (ProcessInfo (..))
import Mit.Push
  ( DidntPushReason (NothingToPush, PushWouldBeRejected, PushWouldntReachRemote, TriedToPush),
    PushResult (DidntPush, Pushed),
    performPush,
    pushResultPushed,
  )
import Mit.Seq1 qualified as Seq1
import Mit.State (MitState (..), readMitState, writeMitState)
import Mit.Undo (Undo (..), concatUndos, undoStash)
import System.Exit (ExitCode (..))
import System.Posix.Terminal (queryTerminal)
import Text.Builder.ANSI qualified as Text

mitCommit ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  Text ->
  Bool ->
  Maybe Text ->
  (Maybe Undo -> IO ()) ->
  IO ()
mitCommit exit output pinfo gitdir allFlag maybeMessage sync = do
  gitMergeInProgress gitdir >>= \case
    False -> mitCommitNotMerge exit output pinfo gitdir allFlag maybeMessage
    True -> mitCommitMerge exit output pinfo gitdir sync

mitCommitMerge ::
  Label ExitCode ->
  Logger Output ->
  Logger ProcessInfo ->
  Text ->
  (Maybe Undo -> IO ()) ->
  IO ()
mitCommitMerge exit output pinfo gitdir sync = do
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

  -- Record that we are no longer merging. FIXME what about undos?
  let state1 =
        MitState
          { head = head1,
            merging = Nothing,
            undo = maybeUndo
          }
  writeMitState gitdir branch state1

  whenJust maybeMerging \source ->
    when (source /= branch) (log output (Output.MergeSucceeded Nothing))

  -- Three possible cases:
  --   1. We had a clean working directory before `mit merge`, so proceed to sync
  --   2. We had a dirty working directory before `mit merge` (evidence: our undo has a `git stash apply` in it)
  --     a. We can cleanly unstash it, so proceed to sync
  --     b. We cannot cleanly unstash it, so don't sync, because that may *further* conflict, and we don't want nasty
  --        double conflict markers

  case maybeUndo >>= undoStash of
    Nothing -> sync (Just (Reset head0 Nothing))
    Just stash -> do
      conflicts <- gitApplyStash pinfo stash
      case List1.nonEmpty conflicts of
        -- FIXME we just unstashed, now we're about to stash again :/
        Nothing -> sync (Just (Reset head0 (Just (Apply stash Nothing))))
        Just conflicts1 -> do
          putPretty $
            Pretty.paragraphs
              [ conflictsStanza "These files are in conflict:" conflicts1,
                Pretty.line $
                  "Resolve the conflicts, then run "
                    <> Pretty.command "mit commit"
                    <> " to synchronize "
                    <> Pretty.branch branch
                    <> " with "
                    <> Pretty.branch ("origin/" <> branch)
                    <> "."
              ]
          when (isJust maybeUndo) (log output Output.CanUndo)

putPretty :: Pretty -> IO ()
putPretty p =
  Pretty.put (emptyLine <> Pretty.indent 2 p <> emptyLine)
  where
    emptyLine = Pretty.line (Pretty.char ' ')

conflictsStanza :: Pretty.Line -> List1 GitConflict -> Pretty
conflictsStanza prefix conflicts =
  Pretty.lines $
    prefix
      : map f (List1.toList conflicts)
  where
    f conflict =
      "  " <> Pretty.style Text.red (prettyGitConflict conflict)

prettyGitConflict :: GitConflict -> Pretty.Line
prettyGitConflict (GitConflict xy name) =
  Pretty.text name <> " (" <> prettyGitConflictXY xy <> ")"

prettyGitConflictXY :: GitConflictXY -> Pretty.Line
prettyGitConflictXY = \case
  AA -> "both added"
  AU -> "added by us"
  DD -> "both deleted"
  DU -> "deleted by us"
  UA -> "added by them"
  UD -> "deleted by them"
  UU -> "both modified"

mitCommitNotMerge :: Label ExitCode -> Logger Output -> Logger ProcessInfo -> Text -> Bool -> Maybe Text -> IO ()
mitCommitNotMerge exit output pinfo gitdir allFlag maybeMessage = do
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

  fetched <- gitFetch pinfo "origin"
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
  pushResult <- performPush pinfo branch maybeHead1 maybeUpstreamHead fetched

  case pushResult of
    DidntPush NothingToPush -> pure ()
    DidntPush (PushWouldBeRejected localCommits numRemoteCommits) ->
      log output (Output.PushWouldBeRejected localCommits numRemoteCommits)
    DidntPush (PushWouldntReachRemote commits) -> log output (Output.PushWouldntReachRemote commits)
    DidntPush (TriedToPush commits) -> log output (Output.PushFailed commits)
    Pushed commits -> log output (Output.PushSucceeded commits)

  whenJust maybeHead1 \head1 -> do
    maybeUndoPush <-
      case pushResult of
        Pushed commits ->
          case Seq1.toList commits of
            [commit] ->
              gitIsMergeCommit pinfo commit.hash <&> \case
                False -> Just (Revert commit.hash Nothing)
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
                    Nothing -> Reset head0 Nothing
                    Just stash -> Reset head0 (Just (Apply stash Nothing))
            (True, False) -> maybeUndoPush
            (True, True) -> do
              -- If we can revert the push *and* there is a stash in the snapshot (i.e. this *isnt* the very first
              -- commit), then we can undo (by reverting then applying the stash).
              --
              -- But if (for example) we can revert the push but there is *not* a stash in the snapshot, that means
              -- there were no commits before this one (`git stash create` is illegal there), so we don't want to
              -- offer to undo, because although we can revert the commit, we have no way of getting from there to
              -- back to having some dirty stuff to commit.
              undoPush <- maybeUndoPush
              stash <- maybeStash
              Just (concatUndos undoPush (Apply stash Nothing))

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
