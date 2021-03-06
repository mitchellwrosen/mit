module Mit.Prelude
  ( module Mit.Prelude,
    module X,
  )
where

import Control.Category as X hiding (id, (.))
import Control.Exception as X hiding (handle)
import Control.Monad as X
import Data.Char as X
import Data.Foldable as X
import Data.Function as X
import qualified Data.List.NonEmpty as List1
import Data.Maybe as X
import Data.Sequence as X (Seq)
import Data.Text as X (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable as X
import Mit.Seq1 as X (Seq1)
import Text.Read as X (readMaybe)
import Prelude as X hiding (head, id)

type List1 =
  List1.NonEmpty

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip fmap

bug :: Text -> a
bug =
  error . Text.unpack

-- FIXME make this faster
int2text :: Integer -> Text
int2text =
  Text.pack . show

putLines :: [Text] -> IO ()
putLines =
  Text.putStr . Text.unlines

quoteText :: Text -> Text
quoteText s =
  if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s

-- FIXME make this faster
text2int :: Text -> Maybe Integer
text2int =
  readMaybe . Text.unpack

onLeftM :: Monad m => (a -> m b) -> m (Either a b) -> m b
onLeftM mx my =
  my >>= either mx pure

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
