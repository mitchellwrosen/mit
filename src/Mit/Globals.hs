module Mit.Globals where

import Mit.Prelude
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

debug :: Bool
debug =
  isJust (unsafePerformIO (lookupEnv "debug"))
{-# NOINLINE debug #-}
