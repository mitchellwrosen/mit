module Mit.Config
  ( verbose,
  )
where

import Mit.Prelude
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

verbose :: Int
verbose =
  unsafePerformIO do
    lookupEnv "MIT_VERBOSE" <&> \case
      Just "1" -> 1
      Just "2" -> 2
      _ -> 0
{-# NOINLINE verbose #-}
