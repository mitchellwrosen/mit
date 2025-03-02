module Mit.Undo
  ( Undo (..),
    undoStash,
    renderUndo,
    parseUndo,
    applyUndo,
  )
where

import Data.Text qualified as Text
import Mit.Git (git, gitUnstageChanges)
import Mit.Logger (Logger)
import Mit.Output (ProcessInfo1)
import Mit.Prelude

data Undo
  = Reset !Text
  | ResetApply !Text !Text
  | Revert !Text
  | RevertApply !Text !Text

undoStash :: Undo -> Maybe Text
undoStash = \case
  Reset _ -> Nothing
  ResetApply _ commit -> Just commit
  Revert _ -> Nothing
  RevertApply _ commit -> Just commit

renderUndo :: Undo -> Text
renderUndo =
  Text.unwords . f
  where
    f :: Undo -> [Text]
    f = \case
      Reset commit -> ["reset", commit]
      ResetApply commit1 commit2 -> ["reset", commit1, "apply", commit2]
      Revert commit -> ["revert", commit]
      RevertApply commit1 commit2 -> ["revert", commit1, "apply", commit2]

parseUndo :: Text -> Maybe Undo
parseUndo =
  f . Text.words
  where
    f :: [Text] -> Maybe Undo
    f = \case
      ["reset", commit1, "apply", commit2] -> Just (ResetApply commit1 commit2)
      ["reset", commit] -> Just (Reset commit)
      ["revert", commit1, "apply", commit2] -> Just (RevertApply commit1 commit2)
      ["revert", commit] -> Just (Revert commit)
      _ -> Nothing

applyUndo :: Logger ProcessInfo1 -> Undo -> IO ()
applyUndo logger = \case
  Reset commit -> applyReset commit
  ResetApply commit1 commit2 -> do
    applyReset commit1
    applyApply commit2
  Revert commit -> applyRevert commit
  RevertApply commit1 commit2 -> do
    applyRevert commit1
    applyApply commit2
  where
    applyApply commit = do
      git @() logger ["stash", "apply", "--quiet", commit]
      gitUnstageChanges logger

    applyReset commit = do
      git @() logger ["clean", "-d", "--force"]
      git @() logger ["reset", "--hard", commit]

    applyRevert commit = git @() logger ["revert", commit]
