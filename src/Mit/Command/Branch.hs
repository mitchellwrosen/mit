module Mit.Command.Branch
  ( mitBranch,
  )
where

import Data.Text qualified as Text
import Mit.Directory
import Mit.Git
import Mit.Monad
import Mit.Prelude
import Mit.Pretty (Pretty)
import Mit.Pretty qualified as Pretty
import Mit.Verbosity (Verbosity)
import Text.Builder.ANSI qualified as Text

mitBranch :: (Abort Pretty) => (Pretty -> Mit r ()) -> Verbosity -> Text -> Mit r ()
mitBranch output verbosity branch = do
  worktreeDir <- do
    rootdir <- io (git verbosity ["rev-parse", "--show-toplevel"])
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  io (gitBranchWorktreeDir verbosity branch) >>= \case
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

      io (git_ verbosity ["worktree", "add", "--detach", worktreeDir])
      line <-
        label \done ->
          cd worktreeDir do
            -- Maybe the branch already exists; try switching to it.
            whenM (io (git verbosity ["switch", branch])) do
              done ("Checked out " <> Pretty.branch branch <> " in " <> Pretty.directory worktreeDir)

            -- Ok, it doesn't exist; create it.
            io (git_ verbosity ["branch", "--no-track", branch])
            io (git_ verbosity ["switch", branch])

            io (gitFetch_ verbosity "origin")
            upstreamExists <- io (gitRemoteBranchExists verbosity "origin" branch)
            if upstreamExists
              then do
                let upstream = "origin/" <> branch
                io (git_ verbosity ["reset", "--hard", "--quiet", upstream])
                io (git_ verbosity ["branch", "--set-upstream-to", upstream])
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
                whenJustM (io (gitDefaultBranch verbosity "origin")) \defaultBranch ->
                  io (git_ verbosity ["reset", "--hard", "--quiet", "origin/" <> defaultBranch])
                pure ("Created " <> Pretty.branch branch <> " in " <> Pretty.directory worktreeDir)
      output (Pretty.line line)
