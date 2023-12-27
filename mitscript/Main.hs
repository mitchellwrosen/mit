module Main where

import Control.Exception (bracket_, throwIO, try)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (createDirectory, removeDirectoryRecursive, withCurrentDirectory)
import System.Environment (setEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (isEOFError)
import System.Process qualified as Process
import Prelude hiding (lex)

main :: IO ()
main =
  bracket_ (createDirectory ".mitscript") (removeDirectoryRecursive ".mitscript") main1

main1 :: IO ()
main1 = do
  createDirectory ".mitscript/remote"
  createDirectory ".mitscript/remote/.git"
  withCurrentDirectory ".mitscript/remote/.git" do
    Process.callCommand "git init --bare --quiet"
  createDirectory ".mitscript/local"
  createDirectory ".mitscript/local/main"
  withCurrentDirectory ".mitscript/local/main" do
    Process.callCommand "git init --quiet"
    Process.callCommand "git remote add origin ../../remote/.git"

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
        withCurrentDirectory (".mitscript/" ++ Text.unpack dir) do
          (code, out, err) <- Process.readCreateProcessWithExitCode (Process.shell (Text.unpack command)) ""
          let suffix = if code == ExitSuccess then " # success" else " # failure"
          Text.putStrLn (dir <> "> " <> command <> suffix)
          when (out /= "") (Text.putStrLn (Text.pack out))
          when (err /= "") (Text.putStrLn (Text.pack err))

data Line
  = Comment !Text
  | Shell !Text !Text

parseLine :: Text -> Line
parseLine line =
  let (before, after) = Text.span (/= '>') line
   in if Text.null after
        then Comment line
        else Shell before (Text.strip (Text.drop 1 after))

forlines :: (Text -> IO ()) -> IO ()
forlines action =
  let loop =
        try Text.getLine >>= \case
          Left ex -> if isEOFError ex then pure () else throwIO ex
          Right line -> do
            action line
            loop
   in loop
