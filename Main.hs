module Main where

import Control.Exception (throwIO)
import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (getArgs)
import System.Exit
import System.IO (Handle, hIsEOF)
import System.Process

main :: IO ()
main =
  getArgs >>= \case
    ["clone", parseGitRepo . Text.pack -> Just (url, name)] -> do
      (_out, _err, code) <- git ["clone", url, "--separate-git-dir", name <> "/.git", name <> "/master"]
      when (code /= ExitSuccess) (throwIO code)
    ["commit"] -> do
      branch <- do
        ([branch], _err, code) <- git ["branch", "--show-current"]
        when (code /= ExitSuccess) (throwIO code)
        pure branch
      do
        (_out, _err, code) <- git ["add", "--all", "--intent-to-add"]
        when (code /= ExitSuccess) (throwIO code)
      do
        code <- git2 ["commit", "--all"]
        when (code /= ExitSuccess) (throwIO code)
      do
        (_out, _err, code) <- git ["push", "origin", branch <> ":" <> branch]
        when (code /= ExitSuccess) (throwIO code)
    -- TODO pop stash
    _ ->
      (Text.putStrLn . Text.unlines)
        [ "Usage:",
          "  mit clone ≪repo≫",
          "  mit commit"
        ]

-- git@github.com:mitchellwrosen/mit.git -> Just ("git@github.com:mitchellwrosen/mit.git", "mit")
parseGitRepo :: Text -> Maybe (Text, Text)
parseGitRepo url = do
  url' <- Text.stripSuffix ".git" url
  pure (url, Text.takeWhileEnd (/= '/') url')

--

git :: [Text] -> IO ([Text], [Text], ExitCode)
git args = do
  do
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
  print (stdoutLines, stderrLines, exitCode)
  pure (stdoutLines, stderrLines, exitCode)
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

git2 :: [Text] -> IO ExitCode
git2 args = do
  do
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
  print exitCode
  pure exitCode
