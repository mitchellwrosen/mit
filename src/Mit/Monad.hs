module Mit.Monad
  ( Mit,
    runMit,
    io,
    getEnv,
    withEnv,
    Goto,
    Label,
    label,
    with,
    with_,
    X,
  )
where

import Control.Monad qualified
import Mit.Prelude

newtype Mit r x a
  = Mit (r -> (a -> IO x) -> IO x)
  deriving stock (Functor)

instance Applicative (Mit r x) where
  pure x = Mit \_ k -> k x
  (<*>) = ap

instance Monad (Mit r x) where
  return = pure
  Mit mx >>= f =
    Mit \r k ->
      mx r (\a -> unMit (f a) r k)

instance MonadIO (Mit r x) where
  liftIO = io

unMit :: Mit r x a -> r -> (a -> IO x) -> IO x
unMit (Mit k) =
  k

runMit :: r -> Mit r a a -> IO a
runMit r m =
  unMit m r pure

return :: x -> Mit r x a
return x =
  Mit \_ _ -> pure x

io :: IO a -> Mit r x a
io m =
  Mit \_ k -> do
    x <- m
    k x

getEnv :: Mit r x r
getEnv =
  Mit \r k -> k r

withEnv :: (r -> s) -> Mit s x a -> Mit r x a
withEnv f m =
  Mit \r k -> unMit m (f r) k

type Goto r x a =
  forall xx void. Label (X x a) xx => a -> Mit r xx void

label :: forall r x a. (Goto r x a -> Mit r (X x a) a) -> Mit r x a
label f =
  Mit \r k -> do
    unX k (unMit (f \a -> return (bury @(X x a) (XR a))) r (pure . XR))

with :: (forall v. (a -> IO v) -> IO v) -> (a -> Mit r (X x b) b) -> Mit r x b
with f action =
  Mit \r k ->
    unX k (f \a -> unMit (action a) r (pure . XR))

with_ :: (forall v. IO v -> IO v) -> Mit r (X x a) a -> Mit r x a
with_ f action =
  Mit \r k ->
    unX k (f (unMit action r (pure . XR)))

-- instance Label (X a b) (X a b)
-- instance Label (X a b) (X (X a b) c)
-- instance Label (X a b) (X (X (X a b) c) d)
-- etc...

data X a b
  = XL a
  | XR b

unX :: (a -> IO b) -> IO (X b a) -> IO b
unX k mx =
  mx >>= \case
    XL b -> pure b
    XR a -> k a

class Label a b where
  bury :: a -> b
  default bury :: a ~ b => a -> b
  bury = id

-- FIXME I don't really think this is correct...
instance {-# INCOHERENT #-} Label (X a b) (X a b)

instance Label (X a b) (X c d) => Label (X a b) (X (X c d) e) where
  bury = XL . bury
