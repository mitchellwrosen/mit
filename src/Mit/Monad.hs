module Mit.Monad
  ( Mit,
    runMit,
    io,
    block,
    getEnv,
    throw,
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

block :: Mit r a a -> Mit r x a
block m =
  Mit \k r -> do
    a <- runMit r m
    k a r

getEnv :: Mit r x r
getEnv =
  Mit \k r -> k r r

throw :: x -> Mit r x a
throw x =
  Mit \_ _ -> pure x
