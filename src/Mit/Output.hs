module Mit.Output
  ( Output (..),
  )
where

import Mit.Prelude

data Output
  = BranchAlreadyCheckedOut !Text !Text -- branch name, directory
  | CheckedOutBranch !Text !Text -- branch name, directory
  | CreatedBranch !Text !Text !(Maybe Text) -- branch name, directory, maybe upstream branch name
  | DirectoryAlreadyExists !Text
  | GitTooOld
  | MergeInProgress
  | NoGitDir
  | NoSuchBranch
  | NotOnBranch
  | NothingToCommit
  | NothingToUndo
  | RemoteIsAhead !Text !Text -- branch name, upstream branch name
