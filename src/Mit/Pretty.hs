module Mit.Pretty
  ( branch,
    command,
    directory,
  )
where

import Mit.Prelude
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import Text.Builder.ANSI qualified as Text.Builder

branch :: Text -> Text.Builder
branch =
  Text.Builder.italic . Text.Builder.text

command :: Text.Builder -> Text.Builder
command =
  Text.Builder.bold . Text.Builder.blue

directory :: Text -> Text.Builder
directory =
  Text.Builder.bold . Text.Builder.text
