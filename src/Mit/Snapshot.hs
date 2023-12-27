module Mit.Snapshot
  ( Snapshot,
    snapshotHead,
    unsafeSnapshotHead,
    snapshotStash,
    performSnapshot,
    undoToSnapshot,
  )
where

import Mit.Env (Env (verbosity))
import Mit.Git (gitCreateStash, gitMaybeHead)
import Mit.Monad (Mit, getEnv, io)
import Mit.Prelude
import Mit.Undo (Undo (Apply, Reset))

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

performSnapshot :: Mit Env Snapshot
performSnapshot = do
  env <- getEnv
  io (gitMaybeHead env.verbosity) >>= \case
    Nothing -> pure Empty
    Just head ->
      io (gitCreateStash env.verbosity) <&> \case
        Nothing -> Clean head
        Just stash -> Dirty head stash

undoToSnapshot :: Snapshot -> [Undo]
undoToSnapshot = \case
  Empty -> []
  Clean head -> [Reset head]
  Dirty head stash -> [Reset head, Apply stash]
