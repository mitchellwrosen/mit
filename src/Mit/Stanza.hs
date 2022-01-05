module Mit.Stanza where

import qualified Data.List as List
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Mit.Builder as Builder
import Mit.Prelude

type Stanza =
  Maybe Text.Builder

renderStanzas :: [Stanza] -> Stanza
renderStanzas stanzas =
  case catMaybes stanzas of
    [] -> Nothing
    stanzas' -> Just (mconcat (List.intersperse (Builder.newline <> Builder.newline) stanzas'))

putStanzas :: [Stanza] -> IO ()
putStanzas stanzas =
  whenJust (renderStanzas stanzas) \s ->
    Text.putStr (Builder.build (Builder.newline <> s <> Builder.newline <> Builder.newline))
