-- | Low-level git commands
module Mit.GitCommand
  ( GitCommand (..),
    FlagQuiet (..),
    git,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Builder.ANSI as Text.Builder
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Ki
import qualified Mit.Builder as Builder
import Mit.Config (verbose)
import Mit.Prelude
import Mit.Process
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals
import System.Posix.Terminal (queryTerminal)
import System.Process
import System.Process.Internals

data GitCommand
  = GitStashApply FlagQuiet Text

renderGitCommand :: GitCommand -> [Text]
renderGitCommand = \case
  GitStashApply quiet commit ->
    ["stash", "apply"] ++ renderFlagQuiet quiet ++ [commit]

data FlagQuiet
  = FlagQuiet
  | NoFlagQuiet

renderFlagQuiet :: FlagQuiet -> [Text]
renderFlagQuiet = \case
  FlagQuiet -> ["--quiet"]
  NoFlagQuiet -> []

------------------------------------------------------------------------------------------------------------------------
-- Git process  stuff

git :: ProcessOutput a => GitCommand -> IO a
git =
  runGit . renderGitCommand

runGit :: ProcessOutput a => [Text] -> IO a
runGit args = do
  let spec :: CreateProcess
      spec =
        CreateProcess
          { child_group = Nothing,
            child_user = Nothing,
            close_fds = True,
            cmdspec = RawCommand "git" (map Text.unpack args),
            create_group = False,
            cwd = Nothing,
            delegate_ctlc = False,
            env = Nothing,
            new_session = False,
            std_err = CreatePipe,
            std_in = NoStream,
            std_out = CreatePipe,
            -- windows-only
            create_new_console = False,
            detach_console = False,
            use_process_jobs = False
          }
  bracket (createProcess spec) cleanup \(_maybeStdin, maybeStdout, maybeStderr, processHandle) ->
    Ki.scoped \scope -> do
      stdoutThread <- Ki.fork scope (drainTextHandle (fromJust maybeStdout))
      stderrThread <- Ki.fork scope (drainTextHandle (fromJust maybeStderr))
      exitCode <- waitForProcess processHandle
      stdoutLines <- Ki.await stdoutThread
      stderrLines <- Ki.await stderrThread
      debugPrintGit args stdoutLines stderrLines exitCode
      fromProcessOutput stdoutLines stderrLines exitCode
  where
    cleanup :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
    cleanup (maybeStdin, maybeStdout, maybeStderr, process) =
      void @_ @ExitCode terminate `finally` closeHandles
      where
        closeHandles :: IO ()
        closeHandles =
          whenJust maybeStdin hClose
            `finally` whenJust maybeStdout hClose
            `finally` whenJust maybeStderr hClose
        terminate :: IO ExitCode
        terminate = do
          withProcessHandle process \case
            ClosedHandle _ -> pure ()
            OpenExtHandle {} -> bug "OpenExtHandle is Windows-only"
            OpenHandle pid -> do
              pgid <- getProcessGroupIDOf pid
              signalProcessGroup sigTERM pgid
          waitForProcess process

debugPrintGit :: [Text] -> Seq Text -> Seq Text -> ExitCode -> IO ()
debugPrintGit args stdoutLines stderrLines exitCode =
  case verbose of
    1 -> Builder.putln (Text.Builder.brightBlack v1)
    2 -> Builder.putln (Text.Builder.brightBlack (v1 <> v2))
    _ -> pure ()
  where
    v1 = Text.Builder.bold (marker <> " git " <> Builder.hcat (map quote args))
    v2 = foldMap (\line -> "\n    " <> Text.Builder.fromText line) (stdoutLines <> stderrLines)

    quote :: Text -> Text.Builder
    quote s =
      if Text.any isSpace s
        then Builder.squoted (Text.Builder.fromText (Text.replace "'" "\\'" s))
        else Text.Builder.fromText s

    marker :: Text.Builder
    marker =
      case exitCode of
        ExitFailure _ -> Text.Builder.singleton '✗'
        ExitSuccess -> Text.Builder.singleton '✓'

drainTextHandle :: Handle -> IO (Seq Text)
drainTextHandle handle = do
  let loop acc =
        hIsEOF handle >>= \case
          False -> do
            line <- Text.hGetLine handle
            loop $! acc Seq.|> line
          True -> pure acc
  loop Seq.empty
