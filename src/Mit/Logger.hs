module Mit.Logger
  ( Logger,
    makeLogger,
    log,
  )
where

import Mit.Prelude

-- | A logger.
newtype Logger a
  = Logger (a -> IO ())

instance Contravariant Logger where
  contramap :: (a -> b) -> Logger b -> Logger a
  contramap f (Logger g) =
    Logger (g . f)

-- | Make a logger.
makeLogger :: (a -> IO ()) -> Logger a
makeLogger =
  Logger

-- | Log with a logger.
log :: Logger a -> a -> IO ()
log (Logger f) =
  f
