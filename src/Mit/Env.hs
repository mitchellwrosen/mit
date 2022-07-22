module Mit.Env
  ( Env (..),
  )
where

import Mit.Prelude

data Env = Env
  { gitdir :: Text,
    verbosity :: Int
  }
