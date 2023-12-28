module Mit.Command.Undo
  ( mitUndo,
  )
where

import Mit.Git (git, gitCurrentBranch)
import Mit.Label (Abort, abort)
import Mit.Logger (Logger)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)
import Mit.State (MitState (..), readMitState)
import Mit.Undo (applyUndo)

-- FIXME output what we just undid
mitUndo :: (Abort Output) => Logger ProcessInfo -> (Logger ProcessInfo -> IO ()) -> IO ()
mitUndo pinfo sync = do
  branch <- gitCurrentBranch pinfo & onNothingM (abort Output.NotOnBranch)
  state <- readMitState pinfo branch
  undo <- state.undo & onNothing (abort Output.NothingToUndo)
  headBefore <- git @Text pinfo ["rev-parse", "HEAD"]
  applyUndo pinfo undo
  headAfter <- git pinfo ["rev-parse", "HEAD"]
  when (headBefore /= headAfter) (sync pinfo)
