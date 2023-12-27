module Mit.Command.Undo
  ( mitUndo,
  )
where

import Data.List.NonEmpty qualified as List1
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
mitUndo logger sync = do
  branch <- gitCurrentBranch logger
  state <- readMitState logger branch
  undos <- List1.nonEmpty state.undos & onNothing (abort Output.NothingToUndo)
  headBefore <- git @Text logger ["rev-parse", "HEAD"]
  for_ undos (applyUndo logger)
  headAfter <- git logger ["rev-parse", "HEAD"]
  when (headBefore /= headAfter) (sync logger)
