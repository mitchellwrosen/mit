module Mit.Stanza
  ( Stanza,
    renderStanzas,
    putStanzas,
  )
where

import Data.List qualified as List
import Mit.Builder qualified as Builder
import Mit.Prelude
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)

type Stanza =
  Maybe Text.Builder

renderStanzas :: [Stanza] -> Stanza
renderStanzas stanzas0 =
  case catMaybes stanzas0 of
    [] -> Nothing
    stanzas ->
      stanzas
        & map ("  " <>)
        & List.intersperse (Builder.newline <> Builder.newline)
        & mconcat
        & Just

putStanzas :: [Stanza] -> IO ()
putStanzas stanzas =
  whenJust (renderStanzas stanzas) \s ->
    Text.Builder.putToStdOut (Builder.newline <> s <> Builder.newline <> Builder.newline)
