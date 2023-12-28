module Mit.Push
  ( performPush,
    PushResult (..),
    DidntPushReason (..),
    pushResultCommits,
    pushResultPushed,
  )
where

import Mit.Git (GitCommitInfo, git, gitCommitsBetween, gitNumCommitsBetween)
import Mit.Label (goto, label)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)
import Mit.Seq1 qualified as Seq1

performPush :: Logger ProcessInfo -> Text -> Maybe Text -> Maybe Text -> Bool -> IO PushResult
performPush logger branch maybeHead maybeUpstreamHead fetched = do
  label \done -> do
    head <- maybeHead & onNothing (goto done (DidntPush NothingToPush))
    commits <- gitCommitsBetween logger maybeUpstreamHead head
    commits1 <- Seq1.fromSeq commits & onNothing (goto done (DidntPush NothingToPush))
    numRemoteCommits <-
      case maybeUpstreamHead of
        Nothing -> pure 0
        Just upstreamHead -> gitNumCommitsBetween logger head upstreamHead
    when (numRemoteCommits > 0) (goto done (DidntPush (PushWouldBeRejected commits1 numRemoteCommits)))
    when (not fetched) (goto done (DidntPush (PushWouldntReachRemote commits1)))
    git logger ["push", "--follow-tags", "--set-upstream", "origin", "--quiet", branch <> ":" <> branch] <&> \case
      False -> DidntPush (TriedToPush commits1)
      True -> Pushed commits1

-- | The result of (considering a) git push.
data PushResult
  = -- | We didn't push anthing.
    DidntPush !DidntPushReason
  | -- | We successfully pushed commits.
    Pushed !(Seq1 GitCommitInfo)

data DidntPushReason
  = -- | There was nothing to push.
    NothingToPush
  | -- | We have commits to push, but we appear to be offline.
    PushWouldntReachRemote !(Seq1 GitCommitInfo)
  | -- | We have commits to push, but there are also remote commits to merge.
    PushWouldBeRejected !(Seq1 GitCommitInfo) !Int
  | -- | We had commits to push, and tried to push, but it failed.
    TriedToPush !(Seq1 GitCommitInfo)

pushResultCommits :: PushResult -> Maybe (Seq1 GitCommitInfo)
pushResultCommits = \case
  DidntPush NothingToPush -> Nothing
  DidntPush (PushWouldntReachRemote commits) -> Just commits
  DidntPush (PushWouldBeRejected commits _) -> Just commits
  DidntPush (TriedToPush commits) -> Just commits
  Pushed commits -> Just commits

pushResultPushed :: PushResult -> Bool
pushResultPushed = \case
  DidntPush _ -> False
  Pushed _ -> True
