module Mit.Snapshot
  ( Snapshot,
    snapshotHead,
    unsafeSnapshotHead,
    snapshotStash,
    undoToSnapshot,
    performSnapshot,
  )
where

import Mit.Git (gitCreateStash, gitMaybeHead)
import Mit.Prelude
import Mit.Undo (Undo (Apply, Reset))
import Mit.Verbosity (Verbosity)

-- | The snapshot of a git repository.
data Snapshot
  = Empty -- empty repo
  | Clean !Text -- head
  | Dirty !Text !Text -- head, stash

snapshotHead :: Snapshot -> Maybe Text
snapshotHead = \case
  Empty -> Nothing
  Clean head -> Just head
  Dirty head _stash -> Just head

unsafeSnapshotHead :: Snapshot -> Text
unsafeSnapshotHead snapshot =
  case snapshotHead snapshot of
    Nothing -> error "unsafeSnapshotHead: no head"
    Just head -> head

snapshotStash :: Snapshot -> Maybe Text
snapshotStash = \case
  Empty -> Nothing
  Clean _head -> Nothing
  Dirty _head stash -> Just stash

undoToSnapshot :: Snapshot -> [Undo]
undoToSnapshot = \case
  Empty -> []
  Clean head -> [Reset head]
  Dirty head stash -> [Reset head, Apply stash]

performSnapshot :: Verbosity -> IO Snapshot
performSnapshot verbosity =
  gitMaybeHead verbosity >>= \case
    Nothing -> pure Empty
    Just head ->
      gitCreateStash verbosity <&> \case
        Nothing -> Clean head
        Just stash -> Dirty head stash
