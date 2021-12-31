-- Some ad-hoc process return value overloading, for cleaner syntax

module Mit.Process where

import qualified Data.Sequence as Seq
import Mit.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.List as List
import qualified Data.Text.Builder.ANSI as Text.Builder
import qualified Data.Text.Lazy.Builder as Text.Builder
import System.Exit (ExitCode (..), exitWith)

logProcess :: Text -> [Text] -> IO ()
logProcess s0 ss =
  Text.putStrLn (Text.Lazy.toStrict (Text.Builder.toLazyText line))
  where
    line = "» " <> Text.Builder.bold (Text.Builder.fromText s0 <> space <> formatArgs ss)
    formatArgs = mconcat . List.intersperse space . map formatArg
    formatArg s = Text.Builder.fromText (if Text.elem ' ' s then s else s)
    space = Text.Builder.singleton ' '

class ProcessOutput a where
  fromProcessOutput :: Seq Text -> Seq Text -> ExitCode -> IO a

instance ProcessOutput () where
  fromProcessOutput _ _ code =
    when (code /= ExitSuccess) (exitWith code)

instance ProcessOutput Bool where
  fromProcessOutput _ _ = \case
    ExitFailure _ -> pure False
    ExitSuccess -> pure True

instance ProcessOutput Text where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    case out of
      Seq.Empty -> throwIO (userError "no stdout")
      line Seq.:<| _ -> pure line

instance a ~ Text => ProcessOutput [a] where
  fromProcessOutput out err code =
    toList @Seq <$> fromProcessOutput out err code

instance a ~ ExitCode => ProcessOutput (Either a Text) where
  fromProcessOutput out _ code =
    case code of
      ExitFailure _ -> pure (Left code)
      ExitSuccess ->
        case out of
          Seq.Empty -> throwIO (userError "no stdout")
          line Seq.:<| _ -> pure (Right line)

instance a ~ Text => ProcessOutput (Maybe a) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    case out of
      Seq.Empty -> pure Nothing
      line Seq.:<| _ -> pure (Just line)

instance a ~ Text => ProcessOutput (Seq a) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    pure out
