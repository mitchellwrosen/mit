-- Some ad-hoc process return value overloading, for cleaner syntax

module Mit.Process where

import Mit.Prelude
import System.Exit (ExitCode (..), exitWith)

class ProcessOutput a where
  fromProcessOutput :: [Text] -> [Text] -> ExitCode -> IO a

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
      [] -> throwIO (userError "no stdout")
      line : _ -> pure line

instance a ~ Text => ProcessOutput [a] where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    pure out

instance a ~ ExitCode => ProcessOutput (Either a Text) where
  fromProcessOutput out _ code =
    case code of
      ExitFailure _ -> pure (Left code)
      ExitSuccess ->
        case out of
          [] -> throwIO (userError "no stdout")
          line : _ -> pure (Right line)
