module Mit.Stanza where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List1
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.ANSI as Text
import qualified Data.Text.Builder.ANSI as Text.Builder
import qualified Data.Text.Encoding.Base64 as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Mit.Builder as Builder
import Mit.Clock (getCurrentTime)
import Mit.Directory
import Mit.Git
import Mit.Prelude
import qualified Mit.Seq as Seq
import qualified Mit.Seq1 as Seq1
import Mit.State
import Mit.Undo
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)

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
