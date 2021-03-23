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
import Data.Text as X (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Traversable as X
import System.IO (Handle, hIsEOF)
import Text.Read as X (readMaybe)
import Prelude as X hiding (head, id)

type List1 =
  List1.NonEmpty

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip fmap

drainTextHandle :: Handle -> IO [Text]
drainTextHandle handle = do
  let loop acc =
        hIsEOF handle >>= \case
          False -> do
            line <- Text.hGetLine handle
            loop (line : acc)
          True -> pure (reverse acc)
  loop []

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

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mx action =
  mx >>= \case
    False -> action
    True -> pure ()

whenM :: Monad m => m Bool -> m () -> m ()
whenM mx action =
  mx >>= \case
    False -> pure ()
    True -> action
