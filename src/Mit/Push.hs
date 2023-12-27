module Mit.Push
  ( performPush,
    PushResult (..),
    DidntPushReason (..),
    pushResultCommits,
    pushResultPushed,
  )
where

import Mit.Git (GitCommitInfo, git, gitCommitsBetween, gitExistCommitsBetween, gitFetch, gitRemoteBranchHead)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)
import Mit.Seq1 qualified as Seq1

-- TODO get context
performPush :: Logger ProcessInfo -> Text -> IO PushResult
performPush logger branch = do
  fetched <- gitFetch logger "origin"
  head <- git logger ["rev-parse", "HEAD"]
  upstreamHead <- gitRemoteBranchHead logger "origin" branch
  commits <- gitCommitsBetween logger upstreamHead head

  case Seq1.fromSeq commits of
    Nothing -> pure (DidntPush NothingToPush)
    Just commits1 -> do
      existRemoteCommits <-
        case upstreamHead of
          Nothing -> pure False
          Just upstreamHead1 -> gitExistCommitsBetween logger head upstreamHead1
      if existRemoteCommits
        then pure (DidntPush (PushWouldBeRejected commits1))
        else
          if fetched
            then do
              let args = ["push", "--follow-tags", "--set-upstream", "origin", "--quiet", branch <> ":" <> branch]
              git logger args <&> \case
                False -> DidntPush (TriedToPush commits1)
                True -> Pushed commits1
            else pure (DidntPush (PushWouldntReachRemote commits1))

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
    PushWouldntReachRemote (Seq1 GitCommitInfo)
  | -- | We have commits to push, but there are also remote commits to merge.
    PushWouldBeRejected (Seq1 GitCommitInfo)
  | -- | We had commits to push, and tried to push, but it failed.
    TriedToPush (Seq1 GitCommitInfo)

pushResultCommits :: PushResult -> Maybe (Seq1 GitCommitInfo)
pushResultCommits = \case
  DidntPush NothingToPush -> Nothing
  DidntPush (PushWouldntReachRemote commits) -> Just commits
  DidntPush (PushWouldBeRejected commits) -> Just commits
  DidntPush (TriedToPush commits) -> Just commits
  Pushed commits -> Just commits

pushResultPushed :: PushResult -> Bool
pushResultPushed = \case
  DidntPush _ -> False
  Pushed _ -> True
