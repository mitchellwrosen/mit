module Mit.Monad
  ( Mit,
    runMit,
    io,
    getEnv,
    throw,
    acquire,
    acquire_,
    block,
  )
where

import Mit.Prelude

newtype Mit r x a
  = Mit ((a -> r -> IO x) -> r -> IO x)
  deriving stock (Functor)

instance Applicative (Mit r x) where
  pure x = Mit \k r -> k x r
  (<*>) = ap

instance Monad (Mit r x) where
  return = pure
  Mit mx >>= f =
    Mit \k ->
      mx (\a -> unMit (f a) k)

instance MonadIO (Mit r x) where
  liftIO = io

unMit :: Mit r x a -> (a -> r -> IO x) -> r -> IO x
unMit (Mit k) = k

runMit :: r -> Mit r a a -> IO a
runMit r m =
  unMit m (\x _ -> pure x) r

io :: IO a -> Mit r x a
io m =
  Mit \k r -> do
    x <- m
    k x r

getEnv :: Mit r x r
getEnv =
  Mit \k r -> k r r

throw :: x -> Mit r x a
throw x =
  Mit \_ _ -> pure x

acquire :: (forall b. (a -> IO b) -> IO b) -> Mit r x a
acquire f =
  Mit \k r ->
    f \a -> k a r

acquire_ :: (forall b. IO b -> IO b) -> Mit r x ()
acquire_ f =
  acquire \k -> f (k ())

block :: Mit r a a -> Mit r x a
block m =
  Mit \k r -> do
    a <- runMit r m
    k a r
