module Mit.State
  ( MitState (..),
    deleteMitState,
    readMitState,
    writeMitState,
  )
where

import Data.Text qualified as Text
import Data.Text.Encoding.Base64 qualified as Text
import Data.Text.IO qualified as Text
import Mit.Label (goto, label)
import Mit.Prelude
import Mit.Undo (Undo (..), parseUndo, renderUndo)
import System.Directory (removeFile)

data MitState = MitState
  { head :: !Text,
    merging :: !(Maybe Text),
    undo :: !(Maybe Undo)
  }

deleteMitState :: Text -> Text -> IO ()
deleteMitState gitdir branch64 = do
  removeFile (makeMitfile gitdir branch64) `catch` \(_ :: IOException) -> pure ()

parseMitState :: Text -> Maybe MitState
parseMitState contents = do
  [headLine, mergingLine, undosLine] <- Just (Text.lines contents)
  ["head", head] <- Just (Text.words headLine)
  merging <-
    case Text.words mergingLine of
      ["merging"] -> Just Nothing
      ["merging", branch] -> Just (Just branch)
      _ -> Nothing
  undo <- do
    undosLine1 <- Text.stripPrefix "undo " undosLine
    if Text.null undosLine1
      then Just Nothing
      else Just <$> parseUndo undosLine1
  pure MitState {head, merging, undo}

readMitState :: Text -> Text -> Text -> IO (Maybe MitState)
readMitState gitdir branch head = do
  label \return -> do
    let mitfile = makeMitfile gitdir branch64
    contents <-
      try (Text.readFile mitfile) >>= \case
        Left (_ :: IOException) -> goto return Nothing
        Right contents -> pure contents
    let maybeState = do
          state <- parseMitState contents
          guard (head == state.head)
          pure state
    case maybeState of
      Nothing -> do
        deleteMitState gitdir branch64
        goto return Nothing
      Just state -> pure (Just state)
  where
    branch64 = Text.encodeBase64 branch

writeMitState :: Text -> Text -> MitState -> IO ()
writeMitState gitdir branch state = do
  let mitfile = makeMitfile gitdir (Text.encodeBase64 branch)
  Text.writeFile mitfile contents `catch` \(_ :: IOException) -> pure ()
  where
    contents :: Text
    contents =
      Text.unlines
        [ "head " <> state.head,
          "merging " <> fromMaybe Text.empty state.merging,
          "undo " <> maybe Text.empty renderUndo state.undo
        ]

makeMitfile :: Text -> Text -> FilePath
makeMitfile gitdir branch64 = do
  Text.unpack (gitdir <> "/.mit-" <> branch64)
