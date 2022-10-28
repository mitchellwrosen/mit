module Mit.Builder
  ( empty,
    hcat,
    newline,
    space,
    squoted,
    vcat,
  )
where

import Data.List qualified as List
import Mit.Prelude
import Text.Builder

empty :: Builder
empty =
  mempty

hcat :: Foldable f => f Builder -> Builder
hcat =
  mconcat . List.intersperse space . toList

newline :: Builder
newline =
  char '\n'

space :: Builder
space =
  char ' '

squote :: Builder
squote =
  char '\''

squoted :: Builder -> Builder
squoted s =
  squote <> s <> squote

vcat :: Foldable f => f Builder -> Builder
vcat =
  mconcat . List.intersperse newline . toList
