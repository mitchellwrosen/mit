module Mit.Command.Status
  ( mitStatus,
  )
where

import Mit.Git (git, gitUnstageChanges)
import Mit.Prelude
import Mit.Pretty qualified as Pretty
import Mit.Verbosity (Verbosity)

mitStatus :: Verbosity -> IO ()
mitStatus verbosity = do
  gitUnstageChanges verbosity
  lines <- git verbosity ["status", "--porcelain=v1"]
  Pretty.put (Pretty.lines (map Pretty.text lines))
