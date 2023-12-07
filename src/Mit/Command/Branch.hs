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
  worktreeDir <- do
    rootdir <- git ["rev-parse", "--show-toplevel"]
    pure (Text.dropWhileEnd (/= '/') rootdir <> branch)

  gitBranchWorktreeDir branch >>= \case
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

      git_ ["worktree", "add", "--detach", worktreeDir]
      line <-
        label \done ->
          cd worktreeDir do
            -- Maybe the branch already exists; try switching to it.
            whenM (git ["switch", branch]) do
              done ("Checked out " <> Pretty.branch branch <> " in " <> Pretty.directory worktreeDir)

            -- Ok, it doesn't exist; create it.
            git_ ["branch", "--no-track", branch]
            git_ ["switch", branch]

            gitFetch_ "origin"
            upstreamExists <- gitRemoteBranchExists "origin" branch
            if upstreamExists
              then do
                let upstream = "origin/" <> branch
                git_ ["reset", "--hard", "--quiet", upstream]
                git_ ["branch", "--set-upstream-to", upstream]
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
                whenJustM (gitDefaultBranch "origin") \defaultBranch ->
                  git_ ["reset", "--hard", "--quiet", "origin/" <> defaultBranch]
                pure ("Created " <> Pretty.branch branch <> " in " <> Pretty.directory worktreeDir)
      output (Pretty.line line)
