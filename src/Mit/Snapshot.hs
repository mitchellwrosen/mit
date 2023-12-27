module Mit.Snapshot
  ( Snapshot,
    snapshotHead,
    snapshotStash,
    undoToSnapshot,
    performSnapshot,
  )
where

import Mit.Git (gitCreateStash, gitMaybeHead)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)
import Mit.Undo (Undo (Apply, Reset))

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

performSnapshot :: Logger ProcessInfo -> IO Snapshot
performSnapshot logger =
  gitMaybeHead logger >>= \case
    Nothing -> pure Empty
    Just head ->
      gitCreateStash logger <&> \case
        Nothing -> Clean head
        Just stash -> Dirty head stash
