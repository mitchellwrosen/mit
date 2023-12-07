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
import Mit.Env (Env (..))
import Mit.Git
import Mit.Monad
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

applyUndo :: Undo -> Mit Env ()
applyUndo = \case
  Apply commit -> do
    env <- getEnv
    io (git_ env.verbosity ["stash", "apply", "--quiet", commit])
    io (gitUnstageChanges env.verbosity)
  Reset commit -> do
    env <- getEnv
    io (git_ env.verbosity ["clean", "-d", "--force"])
    io (git env.verbosity ["reset", "--hard", commit])
  Revert commit -> do
    env <- getEnv
    io (git_ env.verbosity ["revert", commit])

undosStash :: [Undo] -> Maybe Text
undosStash undos =
  listToMaybe [commit | Apply commit <- undos]
