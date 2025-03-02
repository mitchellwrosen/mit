-- | @git merge@ utilities.
module Mit.Merge
  ( performMerge,
    MergeResult (..),
    mergeResultConflicts,
  )
where

import Mit.Git (GitCommitInfo, GitConflict, git, gitCommitsBetween, gitConflicts, gitMergeInProgress)
import Mit.Logger (Logger)
import Mit.Output (ProcessInfo1)
import Mit.Prelude
import Mit.Seq1 qualified as Seq1

-- | @performMerge logger message commit@ merges @commit@ into the current branch, preferring a fast-forward. If a merge
-- commit is created, its commit message is @message@.
performMerge :: Logger ProcessInfo1 -> Text -> Text -> Text -> IO MergeResult
performMerge logger gitdir commit message = do
  head <- git logger ["rev-parse", "HEAD"]
  commits0 <- gitCommitsBetween logger (Just head) commit
  case Seq1.fromSeq commits0 of
    Nothing -> pure NothingToMerge
    Just commits -> do
      git logger ["merge", "--ff", "--no-commit", commit] >>= \case
        False -> do
          conflicts <- gitConflicts logger
          pure (TriedToMerge commits (Seq1.unsafeFromList conflicts))
        True -> do
          -- If this was a fast-forward, a merge would not be in progress at this point.
          whenM (gitMergeInProgress gitdir) (git @() logger ["commit", "--message", message])
          pure (Merged commits)

-- | The result of a 'performMerge'. Lists of commits do not contain the merge commit itself.
data MergeResult
  = -- | There was nothing to merge.
    NothingToMerge
  | -- | We tried to merge these commits, but observed these conflicts.
    TriedToMerge !(Seq1 GitCommitInfo) !(Seq1 GitConflict)
  | -- | We successfully merged these commits.
    Merged !(Seq1 GitCommitInfo) -- note: doesn't distinguish between FF and non-FF

mergeResultConflicts :: MergeResult -> Maybe (Seq1 GitConflict)
mergeResultConflicts = \case
  NothingToMerge -> Nothing
  TriedToMerge _commits conflicts -> Just conflicts
  Merged _commits -> Nothing
