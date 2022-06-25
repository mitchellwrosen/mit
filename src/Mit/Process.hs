-- Some ad-hoc process return value overloading, for cleaner syntax

module Mit.Process
  ( ProcessOutput (..),
  )
where

import Data.Sequence qualified as Seq
import Mit.Prelude
import System.Exit (ExitCode (..), exitWith)

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
