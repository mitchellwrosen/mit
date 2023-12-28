module Mit.Command.Status
  ( mitStatus,
  )
where

import Mit.Git (git, gitUnstageChanges)
import Mit.Logger (Logger)
import Mit.Prelude
import Mit.Pretty qualified as Pretty
import Mit.ProcessInfo (ProcessInfo)

mitStatus :: Logger ProcessInfo -> IO ()
mitStatus pinfo = do
  gitUnstageChanges pinfo
  lines <- git pinfo ["status", "--porcelain=v1"]
  Pretty.put (Pretty.lines (map Pretty.text lines))
