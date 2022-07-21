-- | Low-level git commands
module Mit.GitCommand
  ( Command (..),
    ResetMode (..),
    FlagD (..),
    FlagForce (..),
    FlagIntentToAdd (..),
    FlagNoCommit (..),
    FlagNoFF (..),
    FlagNoRenames (..),
    FlagNoTrack (..),
    FlagQuiet (..),
    FlagVerify (..),
    git,
    git_,
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.ANSI qualified as Text.Builder
import Data.Text.IO qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Ki qualified
import Mit.Builder qualified as Builder
import Mit.Config (verbose)
import Mit.Prelude
import Mit.Process
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hIsEOF)
import System.Posix.Process (getProcessGroupIDOf)
import System.Posix.Signals
import System.Process
import System.Process.Internals

data Command
  = AddAll
  | Add FlagIntentToAdd [Text]
  | Branch FlagNoTrack Text
  | BranchSetUpstreamTo Text
  | BranchShowCurrent
  | Clean FlagD FlagForce
  | Diff FlagQuiet
  | Fetch Text
  | Merge FlagNoCommit FlagNoFF Text
  | MergeAbort
  | Reset ResetMode FlagQuiet Text
  | ResetPaths FlagQuiet [Text]
  | RevParse FlagQuiet FlagVerify Text
  | StashApply FlagQuiet Text
  | StashCreate
  | StatusV1 FlagNoRenames
  | Switch Text
  | SymbolicRef Text

renderCommand :: Command -> [Text]
renderCommand = \case
  Add intentToAdd files -> ["add"] ++ renderFlagIntentToAdd intentToAdd ++ files
  AddAll -> ["add", "--all"]
  Branch noTrack branch -> ["branch"] ++ renderFlagNoTrack noTrack ++ [branch]
  BranchSetUpstreamTo upstream -> ["branch", "--set-upstream-to", upstream]
  BranchShowCurrent -> ["branch", "--show-current"]
  Clean d force -> ["clean"] ++ renderFlagD d ++ renderFlagForce force
  Diff quiet -> ["diff"] ++ renderFlagQuiet quiet
  Fetch remote -> ["fetch", remote]
  Merge noCommit noFF commit -> ["merge"] ++ renderFlagNoCommit noCommit ++ renderFlagNoFF noFF ++ [commit]
  MergeAbort -> ["merge", "--abort"]
  Reset mode quiet commit -> ["reset", renderResetMode mode] ++ renderFlagQuiet quiet ++ [commit]
  ResetPaths quiet paths -> ["reset"] ++ renderFlagQuiet quiet ++ ["--"] ++ paths
  RevParse quiet verify commit -> ["rev-parse"] ++ renderFlagQuiet quiet ++ renderFlagVerify verify ++ [commit]
  StashApply quiet commit -> ["stash", "apply"] ++ renderFlagQuiet quiet ++ [commit]
  StashCreate -> ["stash", "create"]
  StatusV1 noRenames -> ["status"] ++ renderFlagNoRenames noRenames ++ ["--porcelain=v1"]
  Switch branch -> ["switch", branch]
  SymbolicRef commit -> ["symbolic-ref", commit]

data ResetMode
  = Mixed
  | Hard

renderResetMode :: ResetMode -> Text
renderResetMode = \case
  Mixed -> "--mixed"
  Hard -> "--hard"

data FlagD
  = FlagD
  | NoFlagD

renderFlagD :: FlagD -> [Text]
renderFlagD = \case
  FlagD -> ["-d"]
  NoFlagD -> []

data FlagForce
  = FlagForce
  | NoFlagForce

renderFlagForce :: FlagForce -> [Text]
renderFlagForce = \case
  FlagForce -> ["--force"]
  NoFlagForce -> []

data FlagIntentToAdd
  = FlagIntentToAdd
  | NoFlagIntentToAdd

renderFlagIntentToAdd :: FlagIntentToAdd -> [Text]
renderFlagIntentToAdd = \case
  FlagIntentToAdd -> ["--intent-to-add"]
  NoFlagIntentToAdd -> []

data FlagNoCommit
  = FlagNoCommit
  | NoFlagNoCommit

renderFlagNoCommit :: FlagNoCommit -> [Text]
renderFlagNoCommit = \case
  FlagNoCommit -> ["--no-commit"]
  NoFlagNoCommit -> []

data FlagNoFF
  = FlagNoFF
  | NoFlagNoFF

renderFlagNoFF :: FlagNoFF -> [Text]
renderFlagNoFF = \case
  FlagNoFF -> ["--no-ff"]
  NoFlagNoFF -> []

data FlagNoRenames
  = FlagNoRenames
  | NoFlagNoRenames

renderFlagNoRenames :: FlagNoRenames -> [Text]
renderFlagNoRenames = \case
  FlagNoRenames -> ["--no-renames"]
  NoFlagNoRenames -> []

data FlagNoTrack
  = FlagNoTrack
  | NoFlagNoTrack

renderFlagNoTrack :: FlagNoTrack -> [Text]
renderFlagNoTrack = \case
  FlagNoTrack -> ["--no-track"]
  NoFlagNoTrack -> []

data FlagQuiet
  = FlagQuiet
  | NoFlagQuiet

renderFlagQuiet :: FlagQuiet -> [Text]
renderFlagQuiet = \case
  FlagQuiet -> ["--quiet"]
  NoFlagQuiet -> []

data FlagVerify
  = FlagVerify
  | NoFlagVerify

renderFlagVerify :: FlagVerify -> [Text]
renderFlagVerify = \case
  FlagVerify -> ["--verify"]
  NoFlagVerify -> []

------------------------------------------------------------------------------------------------------------------------
-- Git process  stuff

git :: ProcessOutput a => Command -> IO a
git =
  runGit . renderCommand

git_ :: Command -> IO ()
git_ =
  git

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
      stdoutLines <- atomically (Ki.await stdoutThread)
      stderrLines <- atomically (Ki.await stderrThread)
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
