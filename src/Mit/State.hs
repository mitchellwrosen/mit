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
import Mit.Env (Env (..))
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

deleteMitState :: Text -> Mit Env ()
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

readMitState :: Text -> Mit Env (MitState ())
readMitState branch = do
  env <- getEnv
  label \return -> do
    head <-
      io (gitMaybeHead env.verbosity) >>= \case
        Nothing -> return emptyMitState
        Just head -> pure head
    mitfile <- getMitfile branch64
    contents <-
      io (try (Text.readFile mitfile)) >>= \case
        Left (_ :: IOException) -> return emptyMitState
        Right contents -> pure contents
    let maybeState = do
          state <- parseMitState contents
          guard (head == state.head)
          pure state
    state <-
      case maybeState of
        Nothing -> do
          deleteMitState branch64
          return emptyMitState
        Just state -> pure state
    pure (state {head = ()} :: MitState ())
  where
    branch64 = Text.encodeBase64 branch

writeMitState :: Text -> MitState () -> Mit Env ()
writeMitState branch state = do
  env <- getEnv
  head <- io (gitHead env.verbosity)
  let contents :: Text
      contents =
        Text.unlines
          [ "head " <> head,
            "merging " <> fromMaybe Text.empty state.merging,
            "undos " <> showUndos state.undos
          ]
  mitfile <- getMitfile (Text.encodeBase64 branch)
  io (Text.writeFile mitfile contents `catch` \(_ :: IOException) -> pure ())

getMitfile :: Text -> Mit Env FilePath
getMitfile branch64 = do
  env <- getEnv
  gitdir <- io (gitRevParseAbsoluteGitDir env.verbosity)
  pure (Text.unpack (gitdir <> "/.mit-" <> branch64))
