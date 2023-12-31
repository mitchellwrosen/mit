module Mit.Output
  ( Output (..),
  )
where

import Mit.Git (GitCommitInfo, GitConflict)
import Mit.Prelude

data Output
  = BranchAlreadyCheckedOut !Text !Text -- branch name, directory
  | CanUndo
  | CheckedOutBranch !Text !Text -- branch name, directory
  | CreatedBranch !Text !Text !(Maybe Text) -- branch name, directory, maybe upstream branch name
  | DirectoryAlreadyExists !Text
  | GitTooOld
  | MergeInProgress
  | MergeFailed !Text !Text !(Seq1 GitCommitInfo)
  | -- FIXME persist the commits that we're merging so we can report them (no more Just Nothing case)
    MergeSucceeded !Text !Text !(Maybe (Seq1 GitCommitInfo))
  | NoGitDir
  | NoSuchBranch
  | NotOnBranch
  | NothingToCommit
  | NothingToMerge !Text !Text
  | NothingToUndo
  | PullFailed !(Seq1 GitCommitInfo) !(Seq1 GitConflict)
  | PullSucceeded !(Seq1 GitCommitInfo)
  | PushFailed !(Seq1 GitCommitInfo)
  | PushSucceeded !(Seq1 GitCommitInfo)
  | PushWouldBeRejected !(Seq1 GitCommitInfo) !Int
  | PushWouldntReachRemote !(Seq1 GitCommitInfo)
  | UnstashFailed !(Seq1 GitConflict)
  | UpstreamIsAhead !Text !Text -- branch name, upstream branch name
