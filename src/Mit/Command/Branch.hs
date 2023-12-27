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
import Mit.Logger (Logger)
import Mit.ProcessInfo (ProcessInfo)

mitBranch :: (Abort Output) => (Output -> IO ()) -> Logger ProcessInfo -> Text -> IO ()
mitBranch output logger branch = do
  worktreeDir <- do
    rootdir <- git logger ["rev-parse", "--show-toplevel"]
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  gitBranchWorktreeDir logger branch >>= \case
    Just directory -> when (directory /= worktreeDir) (abort (Output.BranchAlreadyCheckedOut branch directory))
    Nothing -> do
      whenM (doesDirectoryExist worktreeDir) (abort (Output.DirectoryAlreadyExists worktreeDir))

      git_ logger ["worktree", "add", "--detach", worktreeDir]

      label \done ->
        cd worktreeDir do
          -- Maybe the branch already exists; try switching to it.
          whenM (git logger ["switch", branch]) do
            output (Output.CheckedOutBranch branch worktreeDir)
            goto done ()

          -- Ok, it doesn't exist; create it.
          git_ logger ["branch", "--no-track", branch]
          git_ logger ["switch", branch]

          gitFetch_ logger "origin"
          upstreamExists <- gitRemoteBranchExists logger "origin" branch
          if upstreamExists
            then do
              let upstream = "origin/" <> branch
              git_ logger ["reset", "--hard", "--quiet", upstream]
              git_ logger ["branch", "--set-upstream-to", upstream]
              output (Output.CreatedBranch branch worktreeDir (Just upstream))
            else do
              -- Start the new branch at the latest origin/main, if there is an origin/main
              -- This seems better than starting from whatever branch the user happened to fork from, which was
              -- probably some slightly out-of-date main
              whenJustM (gitDefaultBranch logger "origin") \defaultBranch ->
                git_ logger ["reset", "--hard", "--quiet", "origin/" <> defaultBranch]
              output (Output.CreatedBranch branch worktreeDir Nothing)
