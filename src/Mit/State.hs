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
import Mit.Git (git, gitMaybeHead, gitRevParseAbsoluteGitDir)
import Mit.Label (goto, label)
import Mit.Prelude
import Mit.Undo (Undo, parseUndos, showUndos)
import Mit.Verbosity (Verbosity)
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

deleteMitState :: Verbosity -> Text -> IO ()
deleteMitState verbosity branch64 = do
  mitfile <- getMitfile verbosity branch64
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
  undos <- Text.stripPrefix "undos " undosLine >>= parseUndos
  pure MitState {head, merging, undos}

readMitState :: Verbosity -> Text -> IO (MitState ())
readMitState verbosity branch = do
  label \return -> do
    head <-
      gitMaybeHead verbosity >>= \case
        Nothing -> goto return emptyMitState
        Just head -> pure head
    mitfile <- getMitfile verbosity branch64
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
          deleteMitState verbosity branch64
          goto return emptyMitState
        Just state -> pure state
    pure (state {head = ()} :: MitState ())
  where
    branch64 = Text.encodeBase64 branch

writeMitState :: Verbosity -> Text -> MitState () -> IO ()
writeMitState verbosity branch state = do
  head <- git verbosity ["rev-parse", "HEAD"]
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "undos " <> showUndos state.undos
          ]
  mitfile <- getMitfile verbosity (Text.encodeBase64 branch)
  Text.writeFile mitfile contents `catch` \(_ :: IOException) -> pure ()

getMitfile :: Verbosity -> Text -> IO FilePath
getMitfile verbosity branch64 = do
  gitdir <- gitRevParseAbsoluteGitDir verbosity
  pure (Text.unpack (gitdir <> "/.mit-" <> branch64))
