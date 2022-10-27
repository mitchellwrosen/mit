module Mit.Prelude
  ( module Mit.Prelude,
    module X,
  )
where

import Control.Applicative as X ((<|>))
import Control.Category as X hiding (id, (.))
import Control.Concurrent.STM as X (atomically)
import Control.Exception as X hiding (handle, throw)
import Control.Monad as X hiding (return)
import Control.Monad.IO.Class as X (MonadIO (..))
import Data.Char as X
import Data.Foldable as X
import Data.Function as X
import Data.Functor as X (($>))
import Data.IORef as X
import Data.List.NonEmpty qualified as List1
import Data.Map as X (Map)
import Data.Maybe as X
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Text as X (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable as X
import Data.Void as X (Void)
import Data.Word as X (Word64)
import GHC.Stack as X (HasCallStack)
import Mit.Seq1 as X (Seq1)
import Text.Read as X (readMaybe)
import Prelude as X hiding (head, id, return)

type List1 =
  List1.NonEmpty

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip fmap

bug :: Text -> a
bug =
  error . Text.unpack

-- FIXME make this faster
word642text :: Word64 -> Text
word642text =
  Text.pack . show

putLines :: [Text] -> IO ()
putLines =
  Text.putStr . Text.unlines

quoteText :: Text -> Text
quoteText s =
  if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s

-- FIXME make this faster
text2word64 :: Text -> Maybe Word64
text2word64 =
  readMaybe . Text.unpack

onLeftM :: Monad m => (a -> m b) -> m (Either a b) -> m b
onLeftM mx my =
  my >>= either mx pure

onNothing :: Applicative m => m a -> Maybe a -> m a
onNothing mx =
  maybe mx pure

onNothingM :: Monad m => m a -> m (Maybe a) -> m a
onNothingM mx my =
  my >>= maybe mx pure

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mx action =
  mx >>= \case
    False -> action
    True -> pure ()

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust =
  for_

whenM :: Monad m => m Bool -> m () -> m ()
whenM mx action =
  mx >>= \case
    False -> pure ()
    True -> action

whenNotM :: Monad m => m Bool -> m () -> m ()
whenNotM mx =
  whenM (not <$> mx)
