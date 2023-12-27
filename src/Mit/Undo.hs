module Mit.Undo
  ( Undo (..),
    concatUndos,
    undoStash,
    renderUndo,
    parseUndo,
    applyUndo,
  )
where

import Data.Text qualified as Text
import Mit.Git (gitUnstageChanges, git_)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)

data Undo
  = Apply !Text !(Maybe Undo) -- apply stash
  | Reset !Text !(Maybe Undo) -- reset to commit
  | Revert !Text !(Maybe Undo) -- revert commit

concatUndos :: Undo -> Undo -> Undo
concatUndos x0 y =
  case x0 of
    Apply commit x -> Apply commit (f x)
    Reset commit x -> Reset commit (f x)
    Revert commit x -> Revert commit (f x)
  where
    f = Just . maybe y (`concatUndos` y)

undoStash :: Undo -> Maybe Text
undoStash = \case
  Apply commit _ -> Just commit
  Reset _ maybeNext -> maybeNext >>= undoStash
  Revert _ maybeNext -> maybeNext >>= undoStash

renderUndo :: Undo -> Text
renderUndo =
  Text.unwords . f
  where
    f :: Undo -> [Text]
    f = \case
      Apply commit next -> ("apply/" <> commit) : maybe [] f next
      Reset commit next -> ("reset/" <> commit) : maybe [] f next
      Revert commit next -> ("revert/" <> commit) : maybe [] f next

parseUndo :: Text -> Maybe Undo
parseUndo =
  join . f . Text.words
  where
    f :: [Text] -> Maybe (Maybe Undo)
    f = \case
      [] -> Just Nothing
      undo : undos
        | Just commit <- Text.stripPrefix "apply/" undo -> Just . Apply commit <$> f undos
        | Just commit <- Text.stripPrefix "reset/" undo -> Just . Reset commit <$> f undos
        | Just commit <- Text.stripPrefix "revert/" undo -> Just . Revert commit <$> f undos
        | otherwise -> Nothing

applyUndo :: Logger ProcessInfo -> Undo -> IO ()
applyUndo logger =
  go
  where
    go :: Undo -> IO ()
    go = \case
      Apply commit next -> do
        git_ logger ["stash", "apply", "--quiet", commit]
        gitUnstageChanges logger
        whenJust next go
      Reset commit next -> do
        git_ logger ["clean", "-d", "--force"]
        git_ logger ["reset", "--hard", commit]
        whenJust next go
      Revert commit next -> do
        git_ logger ["revert", commit]
        whenJust next go
