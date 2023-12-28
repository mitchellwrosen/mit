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
import Mit.Git (gitMaybeHead, gitRevParseAbsoluteGitDir)
import Mit.Label (goto, label)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)
import Mit.Undo (Undo (..), parseUndo, renderUndo)
import System.Directory (removeFile)

data MitState a = MitState
  { head :: !a,
    merging :: !(Maybe Text),
    undo :: !(Maybe Undo)
  }

emptyMitState :: MitState ()
emptyMitState =
  MitState {head = (), merging = Nothing, undo = Nothing}

deleteMitState :: Logger ProcessInfo -> Text -> IO ()
deleteMitState logger branch64 = do
  mitfile <- getMitfile logger branch64
  removeFile mitfile `catch` \(_ :: IOException) -> pure ()

parseMitState :: Text -> Maybe (MitState Text)
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

readMitState :: Logger ProcessInfo -> Text -> IO (MitState ())
readMitState logger branch = do
  label \return -> do
    head <-
      gitMaybeHead logger >>= \case
        Nothing -> goto return emptyMitState
        Just head -> pure head
    mitfile <- getMitfile logger branch64
    contents <-
      try (Text.readFile mitfile) >>= \case
        Left (_ :: IOException) -> goto return emptyMitState
        Right contents -> pure contents
    let maybeState = do
          state <- parseMitState contents
          guard (head == state.head)
          pure state
    state <-
      case maybeState of
        Nothing -> do
          deleteMitState logger branch64
          goto return emptyMitState
        Just state -> pure state
    pure (state {head = ()} :: MitState ())
  where
    branch64 = Text.encodeBase64 branch

writeMitState :: Logger ProcessInfo -> Text -> MitState Text -> IO ()
writeMitState logger branch state = do
  mitfile <- getMitfile logger (Text.encodeBase64 branch)
  Text.writeFile mitfile contents `catch` \(_ :: IOException) -> pure ()
  where
    contents :: Text
    contents =
      Text.unlines
        [ "head " <> state.head,
          "merging " <> fromMaybe Text.empty state.merging,
          "undo " <> maybe Text.empty renderUndo state.undo
        ]

getMitfile :: Logger ProcessInfo -> Text -> IO FilePath
getMitfile logger branch64 = do
  gitdir <- gitRevParseAbsoluteGitDir logger
  pure (Text.unpack (gitdir <> "/.mit-" <> branch64))
