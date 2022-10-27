module Mit.Pretty
  ( branch,
    command,
    directory,
  )
where

import Data.Text.Builder.ANSI qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Mit.Prelude

branch :: Text -> Text.Builder
branch =
  Text.italic . Text.Builder.fromText

command :: Text.Builder -> Text.Builder
command =
  Text.bold . Text.blue

directory :: Text -> Text.Builder
directory =
  Text.bold . Text.Builder.fromText
