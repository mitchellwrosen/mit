module Mit.Builder
  ( empty,
    build,
    hcat,
    newline,
    put,
    putln,
    space,
    squoted,
    vcat,
  )
where

import Data.List qualified as List
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder
import Mit.Prelude

empty :: Builder
empty =
  mempty

build :: Builder -> Text
build =
  Text.Lazy.toStrict . toLazyText

hcat :: Foldable f => f Builder -> Builder
hcat =
  mconcat . List.intersperse space . toList

newline :: Builder
newline =
  singleton '\n'

put :: Builder -> IO ()
put =
  Text.putStr . build

putln :: Builder -> IO ()
putln =
  Text.putStrLn . build

space :: Builder
space =
  singleton ' '

squote :: Builder
squote =
  singleton '\''

squoted :: Builder -> Builder
squoted s =
  squote <> s <> squote

vcat :: Foldable f => f Builder -> Builder
vcat =
  mconcat . List.intersperse newline . toList
