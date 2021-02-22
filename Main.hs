module Main where

import Control.Exception (throwIO)
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (Handle, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Prelude hiding (head)

main :: IO ()
main =
  getArgs >>= \case
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> do
      code <- git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]
      when (code /= ExitSuccess) (throwIO code)
    ["commit"] -> mitCommit
    ["merge", branch] -> mitMerge (Text.pack branch)
    _ ->
      (Text.putStr . Text.unlines)
        [ "Usage:",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit merge ≪branch≫"
        ]

mitCommit :: IO ()
mitCommit = do
  Stdout branch <- git ["branch", "--show-current"]
  gitDiff >>= \case
    Differences -> do
      getBranchRemote branch >>= \case
        -- New branch: push and set upstream
        Nothing -> do
          git2 ["commit", "--patch", "--quiet"]
          git ["push", "--set-upstream"]
        Just remote -> do
          () <- git ["fetch", remote]
          upstream <- getBranchUpstream branch
          git ["rev-list", remote <> "/" <> upstream, Text.cons '^' branch] >>= \case
            Stdout [] -> do
              git2 ["commit", "--patch", "--quiet"]
              -- TODO handle race condition where the push fails with non-fast-forward anyway?
              -- TODO pop stash after? (what stash?)
              git ["push", "--quiet", remote, branch <> ":" <> upstream]
            Stdout _ -> do
              -- FIXME I think this fails on git prior to 2.30.1, if there are any new files
              () <- git ["stash", "push", "--quiet"]
              Stdout head <- git ["rev-parse", "HEAD"]
              let fork :: IO ()
                  fork = do
                    () <- git ["stash", "pop", "--quiet"]
                    git2 ["commit", "--patch", "--quiet"]
                    Text.putStrLn $
                      "Diverged from " <> remote <> "/" <> upstream <> ". Please run \ESC[1mmit sync\ESC[22m."
              gitMerge (remote <> "/" <> upstream) >>= \case
                -- We can't even cleanly merge with upstream, so we're already forked. Might as well allow the commit
                -- locally.
                MergeFailed _conflicts -> do
                  () <- git ["merge", "--abort"]
                  fork
                MergeFastForwarded -> do
                  git ["stash", "pop", "--quiet"] >>= \case
                    ExitFailure _ -> do
                      () <- git ["reset", "--hard", head]
                      fork
                    ExitSuccess ->
                      Text.putStrLn $
                        "Synchronized with " <> remote <> "/" <> upstream <> ". If everything still looks good, please "
                          <> "run \ESC[1mmit commit\ESC[22m."
                MergeBubbled -> do
                  () <- git ["reset", "--hard", head]
                  () <- git ["stash", "pop", "--quiet"]
                  Text.putStrLn "bubbled, then backed out" -- TODO handle this case
    NoDifferences -> exitFailure

mitMerge :: Text -> IO ()
mitMerge branch =
  gitDiff >>= \case
    -- for now: just require a clean worktree
    -- FIXME stash and stuff
    Differences -> die "worktree dirty"
    NoDifferences -> do
      target <-
        getBranchRemote branch >>= \case
          Nothing -> pure branch
          Just remote -> do
            () <- git ["fetch", remote]
            upstream <- Text.append (remote <> "/") <$> getBranchUpstream branch
            -- for now: when merging foo, just require foo and origin/foo to be in sync.
            -- FIXME lift this restriction with some complicated merging and stuff
            git ["rev-list", branch, Text.cons '^' upstream] >>= \case
              Stdout [] -> pure ()
              Stdout _ -> die (branch <> " ahead of " <> upstream)
            git ["rev-list", upstream, Text.cons '^' branch] >>= \case
              Stdout [] -> pure ()
              Stdout _ -> die (upstream <> " ahead of " <> branch)
            pure upstream
      gitMerge target >>= \case
        MergeFailed conflicts -> do
          (Text.putStr . Text.unlines)
            ( "Merge failed. There are conflicts in the following files:" :
              "" :
              map ("  " <>) conflicts
                ++ [ "",
                     "Please either:",
                     "",
                     "  1. Resolve the conflicts, then run \ESC[1mmit commit\ESC[22m.",
                     "  2. Run \ESC[1mgit merge --abort\ESC[22m." -- TODO mit abort
                   ]
            )
          exitFailure
        MergeFastForwarded -> do
          -- TODO pop stash
          pure ()
        MergeBubbled ->
          (Text.putStr . Text.unlines)
            [ "Merge succeeded. Please either:",
              "  1. Run \ESC[1mmit commit\ESC[22m.",
              "  2. Run \ESC[1mgit merge --abort\ESC[22m." -- TODO mit abort
            ]

data DiffResult
  = Differences
  | NoDifferences

gitDiff :: IO DiffResult
gitDiff = do
  () <- git ["reset"]
  () <- git ["add", "--all", "--intent-to-add"]
  git ["diff", "--quiet"] <&> \case
    ExitFailure _ -> Differences
    ExitSuccess -> NoDifferences

data MergeResult
  = MergeFailed [Text]
  | MergeBubbled
  | MergeFastForwarded

gitMerge :: Text -> IO MergeResult
gitMerge branch =
  git ["merge", "--ff", "--no-commit", "--quiet", branch] >>= \case
    ExitFailure _ -> do
      Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      pure (MergeFailed conflicts)
    ExitSuccess ->
      git ["rev-parse", "--quiet", "--verify", "MERGE_HEAD"] >>= \case
        ExitFailure _ -> pure MergeFastForwarded
        ExitSuccess -> pure MergeBubbled

debug :: Bool
debug =
  isJust (unsafePerformIO (lookupEnv "debug"))
{-# NOINLINE debug #-}

die :: Text -> IO a
die message = do
  Text.putStrLn message
  exitFailure

getBranchRemote :: Text -> IO (Maybe Text)
getBranchRemote branch = do
  git ["config", "--local", "--get", "branch." <> branch <> ".remote"] <&> \case
    Left _ -> Nothing
    Right (Stdout remote) -> Just remote

getBranchUpstream :: Text -> IO Text
getBranchUpstream branch = do
  Stdout (Text.stripPrefix "refs/heads/" -> Just upstream) <-
    git ["config", "--local", "--get", "branch." <> branch <> ".merge"]
  pure upstream

-- git@github.com:mitchellwrosen/mit.git -> Just ("git@github.com:mitchellwrosen/mit.git", "mit")
parseGitRepo :: Text -> Maybe (Text, Text)
parseGitRepo url = do
  url' <- Text.stripSuffix ".git" url
  pure (url, Text.takeWhileEnd (/= '/') url')

-- Some ad-hoc process return value overloading, for cleaner syntax

newtype Stdout a
  = Stdout a

newtype Stderr a
  = Stderr a

class ProcessOutput a where
  fromProcessOutput :: [Text] -> [Text] -> ExitCode -> IO a

instance ProcessOutput () where
  fromProcessOutput _ _ code =
    when (code /= ExitSuccess) (throwIO code)

instance ProcessOutput ExitCode where
  fromProcessOutput _ _ = pure

instance ProcessOutput (Stdout Text) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (throwIO code)
    case out of
      [] -> throwIO (userError "no stdout")
      line : _ -> pure (Stdout line)

instance a ~ Text => ProcessOutput (Stdout [a]) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (throwIO code)
    pure (Stdout out)

instance a ~ ExitCode => ProcessOutput (Either a (Stdout Text)) where
  fromProcessOutput out _ code =
    case code of
      ExitFailure _ -> pure (Left code)
      ExitSuccess ->
        case out of
          [] -> throwIO (userError "no stdout")
          line : _ -> pure (Right (Stdout line))

--

git :: ProcessOutput a => [Text] -> IO a
git args = do
  when debug do
    let quote :: Text -> Text
        quote s =
          if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s
    Text.putStrLn (Text.unwords ("git" : map quote args))
  (Nothing, Just stdoutHandle, Just stderrHandle, processHandle) <-
    createProcess
      CreateProcess
        { child_group = Nothing,
          child_user = Nothing,
          close_fds = True,
          cmdspec = RawCommand "git" (map Text.unpack args),
          create_group = False,
          cwd = Nothing,
          delegate_ctlc = False,
          env = Nothing,
          new_session = False,
          std_err = CreatePipe,
          std_in = NoStream,
          std_out = CreatePipe,
          -- windows-only
          create_new_console = False,
          detach_console = False,
          use_process_jobs = False
        }
  exitCode <- waitForProcess processHandle
  stdoutLines <- drainTextHandle stdoutHandle
  stderrLines <- drainTextHandle stderrHandle
  when debug (print (stdoutLines, stderrLines, exitCode))
  fromProcessOutput stdoutLines stderrLines exitCode
  where
    drainTextHandle :: Handle -> IO [Text]
    drainTextHandle handle = do
      let loop acc =
            hIsEOF handle >>= \case
              False -> do
                line <- Text.hGetLine handle
                loop (line : acc)
              True -> pure (reverse acc)
      loop []

-- Yucky interactive/inherity variant (so 'git commit' can open an editor).
git2 :: [Text] -> IO ()
git2 args = do
  when debug do
    let quote :: Text -> Text
        quote s =
          if Text.any isSpace s then "'" <> Text.replace "'" "\\'" s <> "'" else s
    Text.putStrLn (Text.unwords ("git" : map quote args))
  (Nothing, Nothing, Nothing, processHandle) <-
    createProcess
      CreateProcess
        { child_group = Nothing,
          child_user = Nothing,
          close_fds = True,
          cmdspec = RawCommand "git" (map Text.unpack args),
          create_group = False,
          cwd = Nothing,
          delegate_ctlc = True,
          env = Nothing,
          new_session = False,
          std_err = Inherit,
          std_in = Inherit,
          std_out = Inherit,
          -- windows-only
          create_new_console = False,
          detach_console = False,
          use_process_jobs = False
        }
  exitCode <- waitForProcess processHandle
  when debug (print exitCode)
  when (exitCode /= ExitSuccess) (throwIO exitCode)

--

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip fmap
