module Mit.Env
  ( Env (..),
  )
where

import Mit.Prelude

data Env = Env
  { verbosity :: Int
  }
