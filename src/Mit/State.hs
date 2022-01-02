module Mit.State where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Mit.Git
import Mit.Prelude
import Mit.Undo
import System.Directory (removeFile)

data MitState a = MitState
  { head :: a,
    merging :: Maybe Text,
    ranCommitAt :: Maybe Word64,
    undos :: [Undo]
  }
  deriving stock (Eq, Show)

emptyMitState :: MitState ()
emptyMitState =
  MitState {head = (), merging = Nothing, ranCommitAt = Nothing, undos = []}

deleteMitState :: Text -> IO ()
deleteMitState branch64 =
  removeFile (mitfile branch64) `catch` \(_ :: IOException) -> pure ()

parseMitState :: Text -> Maybe (MitState Text)
parseMitState contents = do
  [headLine, mergingLine, ranCommitAtLine, undosLine] <- Just (Text.lines contents)
  ["head", head] <- Just (Text.words headLine)
  merging <-
    case Text.words mergingLine of
      ["merging"] -> Just Nothing
      ["merging", branch] -> Just (Just branch)
      _ -> Nothing
  ranCommitAt <-
    case Text.words ranCommitAtLine of
      ["ran-commit-at"] -> Just Nothing
      ["ran-commit-at", text2word64 -> Just n] -> Just (Just n)
      _ -> Nothing
  undos <- Text.stripPrefix "undos " undosLine >>= parseUndos
  pure MitState {head, merging, ranCommitAt, undos}

readMitState :: Text -> IO (MitState ())
readMitState branch64 = do
  head <- gitHead
  try (Text.readFile (mitfile branch64)) >>= \case
    Left (_ :: IOException) -> pure emptyMitState
    Right contents -> do
      let maybeState = do
            state <- parseMitState contents
            guard (head == state.head)
            pure state
      case maybeState of
        Nothing -> do
          deleteMitState branch64
          pure emptyMitState
        Just state -> pure (state {head = ()} :: MitState ())

writeMitState :: Text -> MitState () -> IO ()
writeMitState branch64 state = do
  head <- gitHead
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "ran-commit-at " <> maybe Text.empty word642text state.ranCommitAt,
            "undos " <> showUndos state.undos
          ]
  Text.writeFile (mitfile branch64) contents `catch` \(_ :: IOException) -> pure ()

mitfile :: Text -> FilePath
mitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)
