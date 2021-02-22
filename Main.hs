module Main where

import Control.Exception (IOException, throwIO, try)
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ANSI qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (Handle, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Read (readMaybe)
import Prelude hiding (head)

-- FIXME careful about --patch for merges :/
-- FIXME stash push -> stash create && reset --hard

main :: IO ()
main = do
  do
    let validations :: [(GitVersion, Text)]
        validations =
          [ ( GitVersion 2 29 0,
              Text.bold "git commit --patch"
                <> " was broken for new files added with "
                <> Text.bold "git add --intent-to-add"
                <> "."
            ),
            ( GitVersion 2 30 1,
              Text.bold "git stash push"
                <> " was broken for new files added with "
                <> Text.bold "git add --intent-to-add"
                <> "."
            )
          ]
    version <- getGitVersion
    case foldr (\(ver, warn) acc -> if version < ver then (ver, warn) : acc else acc) [] validations of
      [] -> pure ()
      warnings ->
        (Text.putStrLn . Text.unlines)
          ( "Warning: your version of " <> Text.bold "git" <> " is buggy." :
            map
              (\(ver, warn) -> "  ∙ Prior to " <> Text.bold ("git version " <> showGitVersion ver <> ", " <> warn))
              warnings
          )
  getArgs >>= \case
    ["abort"] -> mitAbort
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> do
      code <- git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]
      when (code /= ExitSuccess) (throwIO code)
    ["commit"] -> mitCommit
    ["sync"] -> mitSync Nothing
    ["sync", branch] -> mitSync (Just (Text.pack branch))
    _ ->
      (Text.putStr . Text.unlines)
        [ "Usage:",
          "  mit abort",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit sync [≪branch≫]"
        ]

mitAbort :: IO ()
mitAbort = do
  Stdout head <- git ["rev-parse", "HEAD"]
  Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
  try (Text.readFile (Text.unpack (gitdir <> "/" <> head))) >>= \case
    Left (_ :: IOException) -> exitFailure
    Right contents ->
      case Text.words contents of
        [previousHead, stash] -> do
          () <- git ["reset", "--hard", previousHead]
          git ["stash", "apply", "--quiet", stash]
        _ -> exitFailure

mitCommit :: IO ()
mitCommit = do
  gitDiff >>= \case
    Differences -> do
      () <- git ["fetch", "--quiet", "origin"]
      Stdout branch <- git ["branch", "--show-current"]
      git ["show-ref", "--quiet", "--verify", "refs/remotes/origin/" <> branch] >>= \case
        ExitFailure _ -> do
          git2 ["commit", "--patch", "--quiet"]
          git ["push", "--set-upstream", "origin"]
        ExitSuccess -> do
          git ["rev-list", "--max-count", "1", branch <> "..origin/" <> branch] >>= \case
            Stdout [] -> do
              git2 ["commit", "--patch", "--quiet"]
              -- TODO handle race condition where the push fails with non-fast-forward anyway?
              -- TODO pop stash after? (what stash?)
              git ["push", "--quiet", "origin", branch <> ":" <> branch]
            Stdout _ -> do
              () <- git ["stash", "push", "--quiet"]
              let fork :: IO ()
                  fork = do
                    () <- git ["stash", "pop", "--quiet"]
                    git2 ["commit", "--patch", "--quiet"]
                    Text.putStrLn $
                      "Diverged from "
                        <> Text.italic ("origin/" <> branch)
                        <> ". Please run "
                        <> Text.bold "mit sync"
                        <> "."
              Stdout head <- git ["rev-parse", "HEAD"]
              gitMerge head ("origin/" <> branch) >>= \case
                -- We can't even cleanly merge with upstream, so we're already forked. Might as well allow the commit
                -- locally.
                MergeFailed _conflicts -> do
                  () <- git ["merge", "--abort"]
                  fork
                MergeSucceeded commits -> do
                  git ["stash", "pop", "--quiet"] >>= \case
                    ExitFailure _ -> do
                      () <- git ["reset", "--hard", head]
                      fork
                    ExitSuccess -> do
                      (Text.putStr . Text.unlines)
                        ( "Synchronized with " <> Text.italic ("origin/" <> branch) <> ":" :
                          "" :
                          map ("  " <>) commits
                            ++ ["", "If everything still looks good, please run " <> Text.bold "mit commit" <> "."]
                        )
    NoDifferences -> exitFailure

mitSync :: Maybe Text -> IO ()
mitSync maybeBranch = do
  () <- git ["fetch", "--quiet", "origin"]
  target <- do
    branch <-
      case maybeBranch of
        Nothing -> do
          Stdout branch <- git ["branch", "--show-current"]
          pure branch
        Just branch -> do
          -- TODO verify branch is a real target
          pure branch
    git ["show-ref", "--quiet", "--verify", "refs/remotes/origin/" <> branch] <&> \case
      ExitFailure _ ->
        branch
      ExitSuccess -> "origin/" <> branch

  gitDiff >>= \case
    Differences -> do
      Stdout stash <- git ["stash", "create"]
      Stdout head <- git ["rev-parse", "HEAD"]
      () <- git ["reset", "--hard", head]
      gitMerge head target >>= \case
        MergeFailed _conflicts -> do
          () <- git ["merge", "--abort"]
          () <- git ["stash", "apply", "--quiet", stash]
          -- FIXME: handle this case
          Text.putStrLn "merge failed, so backed out"
        MergeSucceeded commits -> do
          git ["stash", "apply", "--quiet", stash] >>= \case
            ExitFailure _ -> do
              -- Record a file for 'mit abort' to use, if invoked. Its name is the current HEAD, i.e. the commit we
              -- advanced to after a successful merge, and its contents are the previous HEAD before attempting the
              -- merge, and the commit that the changes to the working tree were stored in with 'git stash create'.
              Stdout head2 <- git ["rev-parse", "HEAD"]
              Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
              Text.writeFile (Text.unpack (gitdir <> "/" <> head2)) (head <> " " <> stash)

              Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
              -- TODO have 'mit commit' drop the stash
              Text.putStr . Text.unlines $
                "Synchronized with " <> Text.italic target <> ", but there are now conflicts in the following files:" :
                "" :
                map ("  " <>) conflicts
                  ++ [ "",
                       "If you would prefer not to resolve the conflicts at this time, please run "
                         <> Text.bold "mit abort"
                         <> "."
                     ]
            ExitSuccess ->
              unless (null commits) do
                (Text.putStr . Text.unlines)
                  ("Synchronized with " <> Text.italic target <> ":" : "" : map ("  " <>) commits)
    NoDifferences -> do
      Stdout head <- git ["rev-parse", "HEAD"]
      gitMerge head target >>= \case
        MergeFailed conflicts -> do
          (Text.putStr . Text.unlines)
            ( "Failed to synchronize with " <> Text.italic target <> ". There are conflicts in the following files:" :
              "" :
              map ("  " <>) conflicts
                ++ [ "",
                     "Please either:",
                     "",
                     "  ∙ Resolve the conflicts, then run " <> Text.bold "mit commit" <> ".",
                     "  ∙ Run " <> Text.bold "git merge --abort" <> "." -- TODO mit abort
                   ]
            )
          exitFailure
        MergeSucceeded commits -> do
          unless (null commits) do
            (Text.putStr . Text.unlines)
              ("Synchronized with " <> Text.italic target <> ":" : "" : map ("  " <>) commits)
          -- TODO pop stash
          pure ()

data GitVersion
  = GitVersion Int Int Int
  deriving stock (Eq, Ord)

getGitVersion :: IO GitVersion
getGitVersion = do
  Stdout v0 <- git ["--version"]
  fromMaybe (throwIO (userError ("Could not parse git version from: " <> Text.unpack v0))) do
    ["git", "version", v1] <- Just (Text.words v0)
    [sx, sy, sz] <- Just (Text.split (== '.') v1)
    x <- readMaybe (Text.unpack sx)
    y <- readMaybe (Text.unpack sy)
    z <- readMaybe (Text.unpack sz)
    pure (pure (GitVersion x y z))

showGitVersion :: GitVersion -> Text
showGitVersion (GitVersion x y z) =
  Text.pack (show x) <> "." <> Text.pack (show y) <> "." <> Text.pack (show z)

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
  | MergeSucceeded [Text]

gitMerge :: Text -> Text -> IO MergeResult
gitMerge head branch = do
  git ["merge", "--ff", "--quiet", branch] >>= \case
    ExitFailure _ -> do
      Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      pure (MergeFailed conflicts)
    ExitSuccess -> do
      Stdout head2 <- git ["rev-parse", "HEAD"]
      -- --first-parent seems desirable for topic branches
      Stdout commits <-
        git
          [ "rev-list",
            "--color=always",
            "--date=human",
            "--format=format:%C(bold black)%h%C(reset) %C(bold white)%s%C(reset) – %C(italic white)%an%C(reset) %C(italic yellow)%ad%C(reset)",
            "--max-count=10",
            "--max-parents=1", -- don't show merge commits
            head <> ".." <> head2
          ]
      pure (MergeSucceeded (dropEvens commits))
  where
    -- git rev-list with a custom format prefixes every commit with a redundant line :|
    dropEvens :: [a] -> [a]
    dropEvens = \case
      _ : x : xs -> x : dropEvens xs
      xs -> xs

debug :: Bool
debug =
  isJust (unsafePerformIO (lookupEnv "debug"))
{-# NOINLINE debug #-}

die :: Text -> IO a
die message = do
  Text.putStrLn message
  exitFailure

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
