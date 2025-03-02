module Mit.Command.Undo
  ( mitUndo,
  )
where

import Mit.Git (git, gitCurrentBranch, gitMaybeHead)
import Mit.Logger (Logger, log)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.State (MitState (..), readMitState)
import Mit.Undo (applyUndo)
import System.Exit (ExitCode (..))
import UnconditionalJump (Label, goto)

-- FIXME output what we just undid
mitUndo :: Label ExitCode -> Logger Output -> IO () -> Text -> IO ()
mitUndo exit output sync gitdir = do
  let pinfo = Output.ProcessInfo >$< output

  branch <-
    gitCurrentBranch pinfo & onNothingM do
      log output Output.NotOnBranch
      goto exit (ExitFailure 1)
  headBefore <-
    gitMaybeHead pinfo & onNothingM do
      log output Output.NothingToUndo
      goto exit (ExitFailure 1)
  state <-
    readMitState gitdir branch headBefore & onNothingM do
      log output Output.NothingToUndo
      goto exit (ExitFailure 1)
  undo <-
    state.undo & onNothing do
      log output Output.NothingToUndo
      goto exit (ExitFailure 1)
  applyUndo pinfo undo
  headAfter <- git pinfo ["rev-parse", "HEAD"]
  when (headBefore /= headAfter) sync
