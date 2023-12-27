module Mit.Verbosity
  ( Verbosity (..),
    intToVerbosity,
  )
where

import Mit.Prelude

data Verbosity
  = V0
  | V1
  | V2

intToVerbosity :: Int -> Verbosity
intToVerbosity n
  | n <= 0 = V0
  | n == 1 = V1
  | otherwise = V2
