module Mit.Snapshot
  ( Snapshot,
    snapshotStash,
    undoToSnapshot,
    performSnapshot,
  )
where

import Mit.Git (gitCreateStash, gitMaybeHead)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.Output (ProcessInfo1)
import Mit.Undo (Undo (..))

-- | The snapshot of a git repository.
data Snapshot
  = Empty -- empty repo
  | Clean !Text -- head
  | Dirty !Text !Text -- head, stash

snapshotStash :: Snapshot -> Maybe Text
snapshotStash = \case
  Empty -> Nothing
  Clean _head -> Nothing
  Dirty _head stash -> Just stash

undoToSnapshot :: Snapshot -> Maybe Undo
undoToSnapshot = \case
  Empty -> Nothing
  Clean head -> Just (Reset head)
  Dirty head stash -> Just (ResetApply head stash)

performSnapshot :: Logger ProcessInfo1 -> IO Snapshot
performSnapshot pinfo =
  gitMaybeHead pinfo >>= \case
    Nothing -> pure Empty
    Just head ->
      gitCreateStash pinfo <&> \case
        Nothing -> Clean head
        Just stash -> Dirty head stash
