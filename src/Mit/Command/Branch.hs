module Mit.Command.Branch
  ( mitBranch,
  )
where

import Data.Text qualified as Text
import Mit.Directory (cd, doesDirectoryExist)
import Mit.Git (git, gitBranchWorktreeDir, gitDefaultBranch, gitFetch_, gitRemoteBranchExists)
import Mit.Label (Abort, abort, goto, label)
import Mit.Logger (Logger)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)

mitBranch :: (Abort Output) => (Output -> IO ()) -> Logger ProcessInfo -> Text -> IO ()
mitBranch output logger branch = do
  -- Get the worktree directory that corresponds to the branch.
  --
  -- For example, if the main branch (and git repo) is in /my/repo/main, and the branch is called "foo", then the
  -- worktree directory is /my/repo/foo
  worktreeDir <- do
    rootdir <- git logger ["rev-parse", "--show-toplevel"]
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  label \done -> do
    -- It's possible that branch "foo" is already checked out in *some* worktree. If it is, we're done.
    --
    -- If the branch's worktree is already where we were going to create it, this is just a do-nothing no-op. (Possible
    -- improvement: output something here).
    --
    -- If it isn't, that's an error; we'd like to check out "foo" in /my/repo/foo but it's already checked out in
    -- /my/repo/bar?! Complain and quit.
    gitBranchWorktreeDir logger branch & onJustM \directory ->
      if directory == worktreeDir
        then goto done ()
        else abort (Output.BranchAlreadyCheckedOut branch directory)

    -- Maybe branch "foo" isn't checked out in any worktree, but there's already some directory at /my/repo/foo, which
    -- is also a problem.
    whenM (doesDirectoryExist worktreeDir) (abort (Output.DirectoryAlreadyExists worktreeDir))

    -- Create the new worktree with a detached HEAD.
    git @() logger ["worktree", "add", "--detach", worktreeDir]

    -- Inside the new worktree directory...
    cd worktreeDir do
      -- Maybe branch "foo" already exists; try simply switching to it. If that works, we're done!
      whenM (git logger ["switch", branch]) do
        output (Output.CheckedOutBranch branch worktreeDir)
        goto done ()

      -- Ok, it doesn't exist; create it.
      git @() logger ["branch", "--no-track", branch]
      git @() logger ["switch", branch]

      gitFetch_ logger "origin"
      gitRemoteBranchExists logger "origin" branch >>= \case
        False -> do
          -- Start the new branch at the latest origin/main, if there is an origin/main
          -- This seems better than starting from whatever branch the user happened to fork from, which was
          -- probably some slightly out-of-date main
          whenJustM (gitDefaultBranch logger "origin") \defaultBranch ->
            git @() logger ["reset", "--hard", "--quiet", "origin/" <> defaultBranch]
          output (Output.CreatedBranch branch worktreeDir Nothing)
        True -> do
          let upstream = "origin/" <> branch
          git @() logger ["reset", "--hard", "--quiet", upstream]
          git @() logger ["branch", "--set-upstream-to", upstream]
          output (Output.CreatedBranch branch worktreeDir (Just upstream))
