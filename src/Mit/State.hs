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
import Mit.Monad
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

deleteMitState :: Text -> Mit Int x ()
deleteMitState branch64 = do
  mitfile <- getMitfile branch64
  io (removeFile mitfile `catch` \(_ :: IOException) -> pure ())

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

readMitState :: Text -> Mit Int x (MitState ())
readMitState branch = do
  head <- gitHead
  mitfile <- getMitfile branch64
  io (try (Text.readFile mitfile)) >>= \case
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

writeMitState :: Text -> MitState () -> Mit Int x ()
writeMitState branch state = do
  head <- gitHead
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "undos " <> showUndos state.undos
          ]
  mitfile <- getMitfile (Text.encodeBase64 branch)
  io (Text.writeFile mitfile contents `catch` \(_ :: IOException) -> pure ())

getMitfile :: Text -> Mit Int x FilePath
getMitfile branch64 = do
  gitdir <- gitRevParseAbsoluteGitDir
  pure (Text.unpack (gitdir <> "/.mit-" <> branch64))
