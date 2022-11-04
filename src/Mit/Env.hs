module Mit.Env
  ( Env (..),
  )
where

import Mit.Prelude

data Env = Env
  { offline :: Bool,
    verbosity :: Int
  }
