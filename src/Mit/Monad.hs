module Mit.Monad
  ( Mit,
    runMit,
    io,
    getEnv,
    withEnv,
    Goto,
    label,
    with,
    with_,
  )
where

import Control.Monad qualified
import Data.Unique
import Mit.Prelude
import Unsafe.Coerce (unsafeCoerce)

newtype Mit r a
  = Mit (forall x. r -> (a -> IO x) -> IO x)
  deriving stock (Functor)

instance Applicative (Mit r) where
  pure x = Mit \_ k -> k x
  (<*>) = ap

instance Monad (Mit r) where
  return = pure
  Mit mx >>= f =
    Mit \r k ->
      mx r (\a -> unMit (f a) r k)

instance MonadIO (Mit r) where
  liftIO = io

unMit :: Mit r a -> r -> (a -> IO x) -> IO x
unMit (Mit k) =
  k

runMit :: r -> Mit r a -> IO a
runMit r m =
  unMit m r pure

io :: IO a -> Mit r a
io m =
  Mit \_ k -> do
    x <- m
    k x

getEnv :: Mit r r
getEnv =
  Mit \r k -> k r

withEnv :: (r -> s) -> Mit s a -> Mit r a
withEnv f m =
  Mit \r k -> unMit m (f r) k

type Goto r a =
  forall void. a -> Mit r void

label :: (Goto r a -> Mit r a) -> Mit r a
label f =
  Mit \r k -> do
    n <- newUnique
    try (runMit r (f (\x -> io (throwIO (X n x))))) >>= \case
      Left err@(X m y)
        | n == m -> k (unsafeCoerce y)
        | otherwise -> throwIO err
      Right x -> k x

data X = forall a. X Unique a

instance Exception X where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

instance Show X where
  show _ = ""

with :: (forall v. (a -> IO v) -> IO v) -> (a -> Mit r b) -> Mit r b
with f action =
  Mit \r k -> do
    b <- f (\a -> runMit r (action a))
    k b

with_ :: (forall v. IO v -> IO v) -> Mit r a -> Mit r a
with_ f action =
  Mit \r k -> do
    a <- f (runMit r action)
    k a
