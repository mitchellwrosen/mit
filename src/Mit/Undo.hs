-- TODO replace with Git.Command
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

applyUndo :: Undo -> IO ()
applyUndo = \case
  Apply commit -> do
    git_ ["stash", "apply", "--quiet", commit]
    gitUnstageChanges
  Reset commit -> do
    git_ ["clean", "-d", "--force"]
    git ["reset", "--hard", commit]
  Revert commit -> git_ ["revert", commit]

undosStash :: [Undo] -> Maybe Text
undosStash undos =
  listToMaybe [commit | Apply commit <- undos]
