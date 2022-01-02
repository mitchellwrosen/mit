module Mit.Clock
  ( getCurrentTime,
  )
where

import Mit.Prelude
import qualified System.Clock as Clock

getCurrentTime :: IO Word64
getCurrentTime = do
  Clock.TimeSpec {sec, nsec} <- Clock.getTime Clock.Realtime
  pure (fromIntegral (1_000_000_000 * sec + nsec))
