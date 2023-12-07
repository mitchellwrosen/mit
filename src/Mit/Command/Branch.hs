module Mit.Command.Branch
  ( mitBranch,
  )
where

import Data.Text qualified as Text
import Mit.Directory
import Mit.Env
import Mit.Git
import Mit.Monad
import Mit.Prelude
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Text.Builder.ANSI qualified as Text

mitBranch :: (Abort Pretty) => (Pretty -> Mit Env ()) -> Text -> Mit Env ()
mitBranch output branch = do
  env <- getEnv

  worktreeDir <- do
    rootdir <- io (git env.verbosity ["rev-parse", "--show-toplevel"])
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  io (gitBranchWorktreeDir env.verbosity branch) >>= \case
    Just directory ->
      when (directory /= worktreeDir) do
        abort $
          Pretty.line $
            Pretty.style Text.red $
              Pretty.branch branch
                <> " is already checked out in "
                <> Pretty.directory directory
                <> "."
    Nothing -> do
      whenM (doesDirectoryExist worktreeDir) do
        abort $
          Pretty.line (Pretty.style Text.red ("Directory " <> Pretty.directory worktreeDir <> " already exists."))

      io (git_ env.verbosity ["worktree", "add", "--detach", worktreeDir])
      line <-
        label \done ->
          cd worktreeDir do
            -- Maybe the branch already exists; try switching to it.
            whenM (io (git env.verbosity ["switch", branch])) do
              done ("Checked out " <> Pretty.branch branch <> " in " <> Pretty.directory worktreeDir)

            -- Ok, it doesn't exist; create it.
            io (git_ env.verbosity ["branch", "--no-track", branch])
            io (git_ env.verbosity ["switch", branch])

            io (gitFetch_ env.verbosity "origin")
            upstreamExists <- io (gitRemoteBranchExists env.verbosity "origin" branch)
            if upstreamExists
              then do
                let upstream = "origin/" <> branch
                io (git_ env.verbosity ["reset", "--hard", "--quiet", upstream])
                io (git_ env.verbosity ["branch", "--set-upstream-to", upstream])
                pure $
                  "Created "
                    <> Pretty.branch branch
                    <> " in "
                    <> Pretty.directory worktreeDir
                    <> " tracking "
                    <> Pretty.branch upstream
              else do
                -- Start the new branch at the latest origin/main, if there is an origin/main
                -- This seems better than starting from whatever branch the user happened to fork from, which was
                -- probably some slightly out-of-date main
                whenJustM (io (gitDefaultBranch env.verbosity "origin")) \defaultBranch ->
                  io (git_ env.verbosity ["reset", "--hard", "--quiet", "origin/" <> defaultBranch])
                pure ("Created " <> Pretty.branch branch <> " in " <> Pretty.directory worktreeDir)
      output (Pretty.line line)
