module Mit.Undo where

import qualified Data.Text as Text
import Mit.Git
import Mit.Prelude

data Undo
  = Apply Text -- apply stash
  | Reset Text -- reset to commit
  | Revert Text -- revert commit
  deriving stock (Eq, Show)

showUndos :: [Undo] -> Text
showUndos =
  Text.intercalate " " . map showUndo
  where
    showUndo :: Undo -> Text
    showUndo = \case
      Apply commit -> "apply/" <> commit
      Reset commit -> "reset/" <> commit
      Revert commit -> "revert/" <> commit

parseUndos :: Text -> Maybe [Undo]
parseUndos = do
  Text.words >>> traverse parseUndo
  where
    parseUndo :: Text -> Maybe Undo
    parseUndo text =
      asum
        [ Apply <$> Text.stripPrefix "apply/" text,
          Reset <$> Text.stripPrefix "reset/" text,
          Revert <$> Text.stripPrefix "revert/" text,
          error (show text)
        ]

applyUndos :: List1 Undo -> IO ()
applyUndos =
  traverse_ \case
    Apply commit -> void (gitApplyStash commit)
    Reset commit -> gitResetHard commit
    Revert commit -> gitRevert commit
