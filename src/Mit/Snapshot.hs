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
import Mit.ProcessInfo (ProcessInfo)
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
  Clean head -> Just (Reset head Nothing)
  Dirty head stash -> Just (Reset head (Just (Apply stash Nothing)))

performSnapshot :: Logger ProcessInfo -> IO Snapshot
performSnapshot logger =
  gitMaybeHead logger >>= \case
    Nothing -> pure Empty
    Just head ->
      gitCreateStash logger <&> \case
        Nothing -> Clean head
        Just stash -> Dirty head stash
