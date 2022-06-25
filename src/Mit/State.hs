module Mit.State
  ( MitState (..),
    emptyMitState,
    deleteMitState,
    readMitState,
    writeMitState,
  )
where

import Data.Text qualified as Text
import Data.Text.Encoding.Base64 qualified as Text
import Data.Text.IO qualified as Text
import Mit.Git
import Mit.Prelude
import Mit.Undo
import System.Directory (removeFile)

data MitState a = MitState
  { head :: a,
    merging :: Maybe Text,
    undos :: [Undo]
  }
  deriving stock (Eq, Show)

emptyMitState :: MitState ()
emptyMitState =
  MitState {head = (), merging = Nothing, undos = []}

deleteMitState :: Text -> IO ()
deleteMitState branch64 =
  removeFile (mitfile branch64) `catch` \(_ :: IOException) -> pure ()

parseMitState :: Text -> Maybe (MitState Text)
parseMitState contents = do
  [headLine, mergingLine, undosLine] <- Just (Text.lines contents)
  ["head", head] <- Just (Text.words headLine)
  merging <-
    case Text.words mergingLine of
      ["merging"] -> Just Nothing
      ["merging", branch] -> Just (Just branch)
      _ -> Nothing
  undos <- Text.stripPrefix "undos " undosLine >>= parseUndos
  pure MitState {head, merging, undos}

readMitState :: Text -> IO (MitState ())
readMitState branch = do
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
  where
    branch64 = Text.encodeBase64 branch

writeMitState :: Text -> MitState () -> IO ()
writeMitState branch state = do
  head <- gitHead
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "undos " <> showUndos state.undos
          ]
  Text.writeFile (mitfile (Text.encodeBase64 branch)) contents `catch` \(_ :: IOException) -> pure ()

mitfile :: Text -> FilePath
mitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)
