-- TODO replace with Git.Command
module Mit.Undo
  ( Undo (..),
    showUndos,
    parseUndos,
    applyUndo,
    undosStash,
  )
where

import Data.Text qualified as Text
import Mit.Git (git, gitUnstageChanges, git_)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)

data Undo
  = Apply !Text -- apply stash
  | Reset !Text -- reset to commit
  | Revert !Text -- revert commit
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

applyUndo :: Logger ProcessInfo -> Undo -> IO ()
applyUndo logger = \case
  Apply commit -> do
    git_ logger ["stash", "apply", "--quiet", commit]
    gitUnstageChanges logger
  Reset commit -> do
    git_ logger ["clean", "-d", "--force"]
    git logger ["reset", "--hard", commit]
  Revert commit -> git_ logger ["revert", commit]

undosStash :: [Undo] -> Maybe Text
undosStash undos =
  listToMaybe [commit | Apply commit <- undos]
