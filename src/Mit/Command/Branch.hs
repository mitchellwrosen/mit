module Mit.Command.Branch
  ( mitBranch,
  )
where

import Data.Text qualified as Text
import Mit.Directory (cd, doesDirectoryExist)
import Mit.Git
  ( git,
    gitBranchWorktreeDir,
    gitDefaultBranch,
    gitFetch_,
    gitRemoteBranchExists,
    git_,
  )
import Mit.Label (Abort, abort, goto, label)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.Verbosity (Verbosity)

mitBranch :: (Abort Output) => (Output -> IO ()) -> Verbosity -> Text -> IO ()
mitBranch output verbosity branch = do
  worktreeDir <- do
    rootdir <- git verbosity ["rev-parse", "--show-toplevel"]
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  gitBranchWorktreeDir verbosity branch >>= \case
    Just directory -> when (directory /= worktreeDir) (abort (Output.BranchAlreadyCheckedOut branch directory))
    Nothing -> do
      whenM (doesDirectoryExist worktreeDir) (abort (Output.DirectoryAlreadyExists worktreeDir))

      git_ verbosity ["worktree", "add", "--detach", worktreeDir]

      label \done ->
        cd worktreeDir do
          -- Maybe the branch already exists; try switching to it.
          whenM (git verbosity ["switch", branch]) do
            output (Output.CheckedOutBranch branch worktreeDir)
            goto done ()

          -- Ok, it doesn't exist; create it.
          git_ verbosity ["branch", "--no-track", branch]
          git_ verbosity ["switch", branch]

          gitFetch_ verbosity "origin"
          upstreamExists <- gitRemoteBranchExists verbosity "origin" branch
          if upstreamExists
            then do
              let upstream = "origin/" <> branch
              git_ verbosity ["reset", "--hard", "--quiet", upstream]
              git_ verbosity ["branch", "--set-upstream-to", upstream]
              output (Output.CreatedBranch branch worktreeDir (Just upstream))
            else do
              -- Start the new branch at the latest origin/main, if there is an origin/main
              -- This seems better than starting from whatever branch the user happened to fork from, which was
              -- probably some slightly out-of-date main
              whenJustM (gitDefaultBranch verbosity "origin") \defaultBranch ->
                git_ verbosity ["reset", "--hard", "--quiet", "origin/" <> defaultBranch]
              output (Output.CreatedBranch branch worktreeDir Nothing)
