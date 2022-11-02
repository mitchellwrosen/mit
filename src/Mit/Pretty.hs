module Mit.Pretty
  ( Pretty,
    Line,
    put,
    empty,
    line,
    lines,
    paragraphs,
    Mit.Pretty.when,
    Mit.Pretty.whenJust,
    indent,
    style,
    char,
    text,
    builder,
    --
    branch,
    command,
    directory,
  )
where

import Data.List qualified as List
import Data.String (IsString (..))
import Mit.Prelude
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import Text.Builder.ANSI qualified as Text.Builder

type Pretty =
  [Line]

newtype Line
  = Line Text.Builder
  deriving newtype (Semigroup)

instance IsString Line where
  fromString =
    builder . fromString

put :: Pretty -> IO ()
put =
  coerce (Text.Builder.putLnToStdOut . fold . List.intersperse (Text.Builder.char '\n'))

empty :: Pretty
empty =
  []

line :: Line -> Pretty
line x =
  [x]

lines :: [Line] -> Pretty
lines =
  id

paragraphs :: [Pretty] -> Pretty
paragraphs =
  fold . List.intersperse (line (char ' ')) . filter (not . null)

when :: Bool -> Pretty -> Pretty
when = \case
  False -> const empty
  True -> id

whenJust :: Maybe a -> (a -> Pretty) -> Pretty
whenJust = \case
  Nothing -> const empty
  Just x -> \f -> f x

indent :: Int -> Pretty -> Pretty
indent n =
  -- FIXME more efficient pad?
  map (builder (fold (replicate n (Text.Builder.char ' '))) <>)

style :: (Text.Builder -> Text.Builder) -> Line -> Line
style =
  coerce

char :: Char -> Line
char =
  builder . Text.Builder.char

text :: Text -> Line
text =
  builder . Text.Builder.text

builder :: Text.Builder -> Line
builder =
  coerce

---

branch :: Text -> Line
branch =
  builder . Text.Builder.italic . Text.Builder.text

command :: Text -> Line
command =
  builder . Text.Builder.bold . Text.Builder.blue . Text.Builder.text

directory :: Text -> Line
directory =
  builder . Text.Builder.bold . Text.Builder.text
