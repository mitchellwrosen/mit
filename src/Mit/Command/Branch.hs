module Mit.Command.Branch
  ( mitBranch,
  )
where

import Data.Text qualified as Text
import Mit.Directory (cd, doesDirectoryExist)
import Mit.Git (git, gitBranchWorktreeDir, gitDefaultBranch, gitFetch, gitRemoteBranchExists)
import Mit.Label (Abort, abort, goto, label)
import Mit.Logger (Logger, log)
import Mit.Output (Output)
import Mit.Output qualified as Output
import Mit.Prelude
import Mit.ProcessInfo (ProcessInfo)

mitBranch :: (Abort Output) => Logger Output -> Logger ProcessInfo -> Text -> IO ()
mitBranch output pinfo branch = do
  -- Get the worktree directory that corresponds to the branch.
  --
  -- For example, if the main branch (and git repo) is in /my/repo/main, and the branch is called "foo", then the
  -- worktree directory is /my/repo/foo
  worktreeDir <- do
    rootdir <- git pinfo ["rev-parse", "--show-toplevel"]
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  label \done -> do
    -- It's possible that branch "foo" is already checked out in *some* worktree. If it is, we're done.
    --
    -- If the branch's worktree is already where we were going to create it, this is just a do-nothing no-op. (Possible
    -- improvement: output something here).
    --
    -- If it isn't, that's an error; we'd like to check out "foo" in /my/repo/foo but it's already checked out in
    -- /my/repo/bar?! Complain and quit.
    gitBranchWorktreeDir pinfo branch & onJustM \directory ->
      if directory == worktreeDir
        then goto done ()
        else abort (Output.BranchAlreadyCheckedOut branch directory)

    -- Maybe branch "foo" isn't checked out in any worktree, but there's already some directory at /my/repo/foo, which
    -- is also a problem.
    whenM (doesDirectoryExist worktreeDir) (abort (Output.DirectoryAlreadyExists worktreeDir))

    -- Create the new worktree with a detached HEAD.
    git @() pinfo ["worktree", "add", "--detach", worktreeDir]

    -- Inside the new worktree directory...
    cd worktreeDir do
      -- Maybe branch "foo" already exists; try simply switching to it. If that works, we're done!
      whenM (git pinfo ["switch", "--no-guess", "--quiet", branch]) do
        log output (Output.CheckedOutBranch branch worktreeDir)
        goto done ()

      -- Ok, it doesn't exist; create it.
      git @() pinfo ["branch", "--no-track", branch]
      git @() pinfo ["switch", "--quiet", branch]

      _fetched <- gitFetch pinfo "origin"
      gitRemoteBranchExists pinfo "origin" branch >>= \case
        False -> do
          -- Start the new branch at the latest origin/main, if there is an origin/main
          -- This seems better than starting from whatever branch the user happened to fork from, which was
          -- probably some slightly out-of-date main
          whenJustM (gitDefaultBranch pinfo "origin") \defaultBranch ->
            git @() pinfo ["reset", "--hard", "--quiet", "origin/" <> defaultBranch]
          log output (Output.CreatedBranch branch worktreeDir Nothing)
        True -> do
          let upstream = "origin/" <> branch
          git @() pinfo ["reset", "--hard", "--quiet", upstream]
          git @() pinfo ["branch", "--quiet", "--set-upstream-to", upstream]
          log output (Output.CreatedBranch branch worktreeDir (Just upstream))
