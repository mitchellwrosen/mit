{-# LANGUAGE QuasiQuotes #-}
module Mit.Command.Status (mitStatus) where

import Mit.Env (Env)
import Mit.Git (git, gitUnstageChanges)
import Mit.Monad (Mit, io)
import Mit.Prelude
import Mit.Pretty qualified as Pretty

mitStatus :: Mit Env ()
mitStatus = do
  gitUnstageChanges
  lines <- git ["status", "--porcelain=v1"]
  io (Pretty.put (Pretty.lines (map Pretty.text lines)))
