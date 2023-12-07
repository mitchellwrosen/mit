module Mit.Command.Status (mitStatus) where

import Mit.Env (Env (..))
import Mit.Git (git, gitUnstageChanges)
import Mit.Monad (Mit, getEnv, io)
import Mit.Prelude
import Mit.Pretty qualified as Pretty

mitStatus :: Mit Env ()
mitStatus = do
  env <- getEnv
  io (gitUnstageChanges env.verbosity)
  lines <- io (git env.verbosity ["status", "--porcelain=v1"])
  io (Pretty.put (Pretty.lines (map Pretty.text lines)))
