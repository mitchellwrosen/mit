module Mit.State
  ( MitState (..),
    emptyMitState,
    deleteMitState,
    readMitState,
    writeMitState,
    mitStateRanCommitAgo,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import Mit.Clock (getCurrentTime)
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
            "ran-commit-at " <> maybe Text.empty word642text state.ranCommitAt,
            "undos " <> showUndos state.undos
          ]
  Text.writeFile (mitfile (Text.encodeBase64 branch)) contents `catch` \(_ :: IOException) -> pure ()

mitStateRanCommitAgo :: MitState a -> IO Word64
mitStateRanCommitAgo state =
  case state.ranCommitAt of
    Nothing -> pure maxBound
    Just timestamp0 -> do
      timestamp1 <- getCurrentTime
      pure
        if timestamp1 >= timestamp0
          then timestamp1 - timestamp0
          else maxBound -- weird unexpected case

mitfile :: Text -> FilePath
mitfile branch64 =
  Text.unpack (gitdir <> "/.mit-" <> branch64)
