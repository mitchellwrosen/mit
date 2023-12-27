module Mit.ProcessInfo
  ( ProcessInfo (..),
  )
where

import Mit.Prelude
import System.Exit (ExitCode)

-- | Information about a completed process.
data ProcessInfo = ProcessInfo
  { name :: !Text,
    args :: ![Text],
    output :: !(Seq Text),
    errput :: !(Seq Text),
    exitCode :: !ExitCode,
    seconds :: !Double
  }
