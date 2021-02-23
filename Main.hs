module Main where

import Control.Exception (IOException, catch, throwIO, try)
import Control.Monad
import Data.Char
import Data.IORef
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.ANSI qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (removeFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (Handle, hIsEOF)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Read (readMaybe)
import Prelude hiding (head)

-- FIXME careful about --patch for merges :/
-- FIXME stash push -> stash create && reset --hard
-- FIXME show commit summary

main :: IO ()
main = do
  warnIfBuggyGit
  getArgs >>= \case
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> do
      code <- git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]
      when (code /= ExitSuccess) (exitWith code)
    ["commit"] -> mitCommit
    ["sync"] -> mitSync Nothing
    ["sync", branch] -> mitSync (Just (Text.pack branch))
    ["undo"] -> mitUndo
    _ ->
      putLines
        [ "Usage:",
          "  mit clone ≪repo≫",
          "  mit commit",
          "  mit sync [≪branch≫]",
          "  mit undo"
        ]
  where
    warnIfBuggyGit :: IO ()
    warnIfBuggyGit = do
      version <- gitVersion
      case foldr (\(ver, warn) acc -> if version < ver then (ver, warn) : acc else acc) [] validations of
        [] -> pure ()
        warnings ->
          putLines $
            map
              ( \(ver, warn) ->
                  Text.yellow ("Prior to " <> Text.bold "git" <> " version " <> showGitVersion ver <> ", " <> warn)
              )
              warnings
      where
        validations :: [(GitVersion, Text)]
        validations =
          [ ( GitVersion 2 29 0,
              Text.bold "git commit --patch"
                <> " was broken for new files added with "
                <> Text.bold "git add --intent-to-add"
                <> "."
            ),
            ( GitVersion 2 30 1,
              Text.bold "git stash create"
                <> " was broken for new files added with "
                <> Text.bold "git add --intent-to-add"
                <> "."
            )
          ]

mitUndo :: IO ()
mitUndo = do
  Stdout head <- git ["rev-parse", "HEAD"]
  Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
  try (Text.readFile (Text.unpack (gitdir <> "/mit-" <> head))) >>= \case
    Left (_ :: IOException) -> exitFailure
    Right contents ->
      case Text.words contents of
        [previousHead] -> do
          () <- git ["reset", "--hard", "--quiet", previousHead]
          removeFile (Text.unpack (gitdir <> "/mit-" <> head)) `catch` \(_ :: IOException) -> pure ()
        [previousHead, stash] -> do
          () <- git ["reset", "--hard", "--quiet", previousHead]
          () <- git ["stash", "apply", "--quiet", stash]
          removeFile (Text.unpack (gitdir <> "/mit-" <> head)) `catch` \(_ :: IOException) -> pure ()
        _ -> exitFailure

mitCommit :: IO ()
mitCommit = do
  git ["merge", "--quiet", "HEAD"] >>= \case
    ExitFailure _ -> exitFailure -- FIXME better feedback, or perhaps just do the commit.
    ExitSuccess -> pure ()
  gitDiff >>= \case
    Differences -> pure ()
    -- TODO do some sort of "mit status" here?
    NoDifferences -> exitFailure
  gitFetch
  Stdout branch <- git ["branch", "--show-current"]
  git ["show-ref", "--quiet", "--verify", "refs/remotes/origin/" <> branch] >>= \case
    ExitFailure _ -> do
      Stdout head <- git ["rev-parse", "HEAD"]
      git2 ["commit", "--patch", "--quiet"]
      Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
      removeFile (Text.unpack (gitdir <> "/mit-" <> head)) `catch` \(_ :: IOException) -> pure ()
      git ["push", "--set-upstream", "origin"]
    ExitSuccess ->
      git ["rev-list", "--max-count", "1", branch <> "..origin/" <> branch] >>= \case
        Stdout [] -> do
          Stdout head <- git ["rev-parse", "HEAD"]
          git2 ["commit", "--patch", "--quiet"]
          Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
          removeFile (Text.unpack (gitdir <> "/mit-" <> head)) `catch` \(_ :: IOException) -> pure ()
          git ["push", "--quiet", "origin", branch <> ":" <> branch]
        Stdout _ -> do
          () <- git ["stash", "push", "--quiet"]
          Stdout head <- git ["rev-parse", "HEAD"]
          gitMerge head ("origin/" <> branch) >>= \case
            MergeFailed _conflicts -> do
              () <- git ["merge", "--abort"]
              () <- git ["stash", "pop", "--quiet"]
              git2 ["commit", "--patch", "--quiet"]
              Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
              removeFile (Text.unpack (gitdir <> "/mit-" <> head)) `catch` \(_ :: IOException) -> pure ()
              Stdout head2 <- git ["rev-parse", "HEAD"]
              -- TODO dont bother if head == target
              gitMerge head2 ("origin/" <> branch) >>= \case
                -- exit code 0, because the commit worked
                MergeFailed conflicts ->
                  putLines (syncFailedMessage ("origin/" <> branch) conflicts)
                -- This is an unexpected but technically possible case (I think). Merging with upstream failed at
                -- first, but then succeeded after committing locally.
                MergeSucceeded commits -> do
                  putLines (syncMessage ("origin/" <> branch) commits [])
                  git ["push", "--quiet", "origin", branch <> ":" <> branch]
            MergeSucceeded commits -> do
              git ["stash", "pop", "--quiet"] >>= \case
                ExitFailure _ -> do
                  () <- git ["reset", "--hard", "--quiet", head]
                  () <- git ["stash", "pop", "--quiet"]
                  git2 ["commit", "--patch", "--quiet"]
                  Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
                  removeFile (Text.unpack (gitdir <> "/mit-" <> head)) `catch` \(_ :: IOException) -> pure ()
                  Stdout head2 <- git ["rev-parse", "HEAD"]
                  -- TODO dont bother if head == target
                  gitMerge head2 ("origin/" <> branch) >>= \case
                    -- exit code 0, because the commit worked
                    MergeFailed conflicts ->
                      putLines (syncFailedMessage ("origin/" <> branch) conflicts)
                    -- impossible: we just observed conflicts with this commit and upstream
                    MergeSucceeded _ -> undefined
                ExitSuccess ->
                  putLines $
                    syncMessage ("origin/" <> branch) commits []
                      ++ [ "",
                           "If everything still looks good, please run "
                             <> Text.bold (Text.blue "mit commit")
                             <> "."
                         ]

mitSync :: Maybe Text -> IO ()
mitSync maybeBranch = do
  git ["merge", "--quiet", "HEAD"] >>= \case
    ExitFailure _ -> exitFailure
    ExitSuccess -> pure ()
  gitFetch
  (target, targetHash :: Text) <- do
    branch <-
      case maybeBranch of
        Nothing -> do
          Stdout branch <- git ["branch", "--show-current"]
          pure branch
        Just branch -> pure branch
    git ["rev-parse", "refs/remotes/origin/" <> branch] >>= \case
      Left _ -> do
        when (isNothing maybeBranch) exitSuccess -- sync with self
        git ["rev-parse", branch] >>= \case
          Left _ -> exitFailure -- sync with non-existent
          Right (Stdout targetHash) -> pure (branch, targetHash)
      Right (Stdout targetHash) -> pure ("origin/" <> branch, targetHash)
  gitDiff >>= \case
    Differences -> do
      Stdout head <- git ["rev-parse", "HEAD"]
      unless (head == targetHash) do
        Stdout stash <- git ["stash", "create"]
        () <- git ["reset", "--hard", "--quiet", head]
        gitMerge head target >>= \case
          MergeFailed mergeConflicts -> do
            () <- git ["add", "--all"]
            -- TODO better commit message than the default
            () <- git ["commit", "--no-edit", "--quiet"]
            head2 <- recordUndoFile head (Just stash)
            commits <- prettyCommitsBetween head head2
            git ["stash", "apply", "--quiet", stash] >>= \case
              ExitFailure _ -> do
                Stdout stashConflicts <- git ["diff", "--name-only", "--diff-filter=U"]
                putLines $
                  syncMessage target commits (List.nub (stashConflicts ++ mergeConflicts)) ++ ["", undoMessage]
                git ["reset", "--quiet"] -- unmerged (weird) -> unstaged (normal)
              ExitSuccess ->
                putLines (syncMessage target commits mergeConflicts ++ ["", undoMessage])
            exitFailure
          MergeSucceeded commits -> do
            _ <- recordUndoFile head (Just stash)
            git ["stash", "apply", "--quiet", stash] >>= \case
              ExitFailure _ -> do
                Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
                putLines (syncMessage target commits conflicts ++ ["", undoMessage])
                git ["reset", "--quiet"] -- unmerged (weird) -> unstaged (normal)
              ExitSuccess -> putLines (syncMessage target commits [] ++ ["", undoMessage])
    NoDifferences -> do
      Stdout head <- git ["rev-parse", "HEAD"]
      unless (head == targetHash) do
        gitMerge head target >>= \case
          -- FIXME: nicer "git status" story here. the fact that there are conflict markers isn't obvious
          MergeFailed conflicts -> do
            () <- git ["add", "--all"]
            -- TODO better commit message than the default
            () <- git ["commit", "--no-edit", "--quiet"]
            head2 <- recordUndoFile head Nothing
            commits <- prettyCommitsBetween head head2
            putLines (syncMessage target commits conflicts ++ ["", undoMessage])
            exitFailure
          MergeSucceeded commits ->
            -- FIXME allow undo here
            putLines (syncMessage target commits [])

-- Record a file for 'mit undo' to use, if invoked. Its name is 'mit-' plus the current HEAD, and its
-- previous HEAD to undo to, plus an optional stash to apply after that.
recordUndoFile :: Text -> Maybe Text -> IO Text
recordUndoFile previousHead maybeStash = do
  Stdout head <- git ["rev-parse", "HEAD"]
  Stdout gitdir <- git ["rev-parse", "--absolute-git-dir"]
  Text.writeFile (Text.unpack (gitdir <> "/mit-" <> head)) contents
  pure head
  where
    contents :: Text
    contents =
      previousHead <> maybe "" (" " <>) maybeStash

syncMessage :: Text -> [Text] -> [Text] -> [Text]
syncMessage target commits conflicts =
  appendConflictsMessage commitsMessage
  where
    commitsMessage :: [Text]
    commitsMessage =
      "Synchronized with " <> Text.italic target <> "." :
      "" :
      map ("  " <>) commits
    appendConflictsMessage :: [Text] -> [Text]
    appendConflictsMessage =
      if null conflicts
        then id
        else
          ( ++
              ( "" :
                "The following files have conflicts." :
                "" :
                map (("  " <>) . Text.red) conflicts
              )
          )

-- FIXME delete
syncFailedMessage :: Text -> [Text] -> [Text]
syncFailedMessage target conflicts =
  "Failed to synchronize with " <> Text.italic target <> "." :
  "" :
  map (("  " <>) . Text.red) conflicts
    ++ [ "",
         "If you would prefer not to resolve the conflicts at this time, please run",
         Text.bold (Text.blue "mit abort")
           <> ". Otherwise, please resolve the conflicts, then run "
           <> Text.bold (Text.blue "mit commit")
           <> "."
       ]

undoMessage :: Text
undoMessage =
  "Run " <> Text.bold (Text.blue "mit undo") <> " to undo this change."

data DiffResult
  = Differences
  | NoDifferences

gitDiff :: IO DiffResult
gitDiff = do
  () <- git ["reset", "--quiet"]
  () <- git ["add", "--all", "--intent-to-add"]
  git ["diff", "--quiet"] <&> \case
    ExitFailure _ -> Differences
    ExitSuccess -> NoDifferences

-- | git fetch origin, at most once per run.
gitFetch :: IO ()
gitFetch = do
  fetched <- readIORef fetchedRef
  unless fetched do
    () <- git ["fetch", "--quiet", "origin"]
    writeIORef fetchedRef True

fetchedRef :: IORef Bool
fetchedRef =
  unsafePerformIO (newIORef False)
{-# NOINLINE fetchedRef #-}

data MergeResult
  = MergeFailed [Text]
  | MergeSucceeded [Text] -- FIXME remove [Text]

gitMerge :: Text -> Text -> IO MergeResult
gitMerge head target = do
  git ["merge", "--ff", "--quiet", target] >>= \case
    ExitFailure _ -> do
      Stdout conflicts <- git ["diff", "--name-only", "--diff-filter=U"]
      pure (MergeFailed conflicts)
    ExitSuccess -> do
      Stdout head2 <- git ["rev-parse", "HEAD"]
      MergeSucceeded <$> prettyCommitsBetween head head2

prettyCommitsBetween :: Text -> Text -> IO [Text]
prettyCommitsBetween commit1 commit2 =
  if commit1 == commit2
    then pure []
    else do
      Stdout commits <-
        -- --first-parent seems desirable for topic branches
        git
          [ "rev-list",
            "--color=always",
            "--date=human",
            "--format=format:%C(bold black)%h%C(reset) %C(bold white)%s%C(reset) – %C(italic white)%an%C(reset) %C(italic yellow)%ad%C(reset)",
            "--max-count=10",
            "--max-parents=1", -- don't show merge commits
            commit1 <> ".." <> commit2
          ]
      pure (dropEvens commits)
  where
    -- git rev-list with a custom format prefixes every commit with a redundant line :|
    dropEvens :: [a] -> [a]
    dropEvens = \case
      _ : x : xs -> x : dropEvens xs
      xs -> xs

data GitVersion
  = GitVersion Int Int Int
  deriving stock (Eq, Ord)

gitVersion :: IO GitVersion
gitVersion = do
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
    when (code /= ExitSuccess) (exitWith code)

instance ProcessOutput ExitCode where
  fromProcessOutput _ _ = pure

instance ProcessOutput (Stdout Text) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
    case out of
      [] -> throwIO (userError "no stdout")
      line : _ -> pure (Stdout line)

instance a ~ Text => ProcessOutput (Stdout [a]) where
  fromProcessOutput out _ code = do
    when (code /= ExitSuccess) (exitWith code)
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
  when (exitCode /= ExitSuccess) (exitWith exitCode)

--

putLines :: [Text] -> IO ()
putLines =
  Text.putStr . Text.unlines

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) =
  flip fmap
