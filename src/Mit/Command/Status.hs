module Mit.Command.Status
  ( mitStatus,
  )
where

import Mit.Git (git, gitUnstageChanges)
import Mit.Logger (Logger)
import Mit.Output (ProcessInfo1)
import Mit.Prelude
import Mit.Pretty qualified as Pretty

mitStatus :: Logger ProcessInfo1 -> IO ()
mitStatus pinfo = do
  gitUnstageChanges pinfo
  lines <- git pinfo ["status", "--porcelain=v1"]
  Pretty.put (Pretty.lines (map Pretty.text lines))
