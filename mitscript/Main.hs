module Main where

import Control.Exception
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.IO.Error (isEOFError)
import System.IO.Temp
import System.Process qualified as Process
import Prelude hiding (lex)
import System.Environment

main :: IO ()
main = do
  withSystemTempDirectory "mitscript" \tempdir -> do
    createDirectory (tempdir </> "remote")
    createDirectory (tempdir </> "remote" </> ".git")
    withCurrentDirectory (tempdir </> "remote" </> ".git") do
      Process.callCommand "git init --bare --quiet"
    createDirectory (tempdir </> "local")
    createDirectory (tempdir </> "local" </> "main")
    withCurrentDirectory (tempdir </> "local" </> "main") do
      Process.callCommand "git init --quiet"
      Process.callCommand ("git remote add origin " ++ (tempdir </> "remote" </> ".git"))

      -- Deterministic commit hashes
      setEnv "GIT_AUTHOR_NAME" "Bob Ross"
      setEnv "GIT_AUTHOR_EMAIL" "bob@bobross.com"
      setEnv "GIT_AUTHOR_DATE" "1970-01-01T00:00:00"
      setEnv "GIT_COMMITTER_NAME" "Bob Ross"
      setEnv "GIT_COMMITTER_EMAIL" "bob@bobross.com"
      setEnv "GIT_COMMITTER_DATE" "1970-01-01T00:00:00"

      forlines \line0 -> do
        case parseLine line0 of
          Comment line -> Text.putStrLn line
          Shell dir command ->
            withCurrentDirectory (tempdir </> Text.unpack dir) do
              (code, out, err) <- Process.readCreateProcessWithExitCode (Process.shell (Text.unpack command)) ""
              let prefix = if code == ExitSuccess then "✓ " else "✗ "
              Text.putStrLn (prefix <> dir <> " > " <> command)
              when (out /= "") (Text.putStrLn (Text.pack out))
              when (err /= "") (Text.putStrLn (Text.pack err))

data Line
  = Comment Text
  | Shell Text Text

parseLine :: Text -> Line
parseLine line =
  case Text.words line of
    dir : ">" : command -> Shell dir (Text.unwords command)
    _ -> Comment line

forlines :: (Text -> IO ()) -> IO ()
forlines action =
  let loop =
        try Text.getLine >>= \case
          Left ex -> if isEOFError ex then pure () else throwIO ex
          Right line -> do
            action line
            loop
   in loop
