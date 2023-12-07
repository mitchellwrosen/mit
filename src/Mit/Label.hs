{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Mit.Label
  ( -- * Labels
    Label,
    label,
    goto,

    -- ** Derived @label@ variants
    labelNothing,
    labelJust,
    labelLeft,
    labelRight,

    -- ** Implicit labels
    Abort,
    stick,
    abort,
  )
where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, catch, throwIO)
import Data.Functor.Contravariant (Contravariant (contramap))
import IntSupply (IntSupply)
import IntSupply qualified
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

newtype Label a
  = Label (forall x. a -> IO x)

instance Contravariant Label where
  contramap f (Label g) =
    Label (g . f)

label :: (Label a -> IO a) -> IO a
label f = do
  i <- IntSupply.next supply
  catch (f (Label (throwIO . X i))) \err@(X j x) ->
    if i == j
      then pure (unsafeCoerce x)
      else throwIO err

goto :: Label a -> a -> IO x
goto (Label f) x =
  f x

labelNothing :: (Label () -> IO a) -> IO (Maybe a)
labelNothing f =
  label (fmap Just . f . contramap (\() -> Nothing))

labelJust :: (Label a -> IO ()) -> IO (Maybe a)
labelJust f =
  label (fmap (\() -> Nothing) . f . contramap Just)

labelLeft :: (Label a -> IO b) -> IO (Either a b)
labelLeft f =
  label (fmap Right . f . contramap Left)

labelRight :: (Label b -> IO a) -> IO (Either a b)
labelRight f =
  label (fmap Left . f . contramap Right)

type Abort a =
  (?abort :: Label a)

-- | \"Stick\" a label @l@, making any @abort x@ call in the given argument equivalent to @goto l x@.
stick :: Label a -> ((Abort a) => b) -> b
stick g x = let ?abort = g in x

-- | Abort to the stuck label.
abort :: (Abort a) => a -> IO x
abort x =
  case ?abort of
    Label f -> f x

data X = forall a. X {-# UNPACK #-} !Int a

instance Exception X where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show X where
  show _ = "<<internal crio exception>>"

supply :: IntSupply
supply =
  unsafePerformIO IntSupply.new
{-# NOINLINE supply #-}
