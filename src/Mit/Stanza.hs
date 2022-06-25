module Mit.Stanza
  ( Stanza,
    renderStanzas,
    putStanzas,
  )
where

import Data.List qualified as List
import Data.Text.IO qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Mit.Builder qualified as Builder
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
