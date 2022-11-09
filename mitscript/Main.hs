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

main :: IO ()
main = do
  withSystemTempDirectory "mitscript" \tempdir -> do
    putStrLn tempdir
    createDirectory (tempdir </> "remote")
    createDirectory (tempdir </> "remote" </> ".git")
    withCurrentDirectory (tempdir </> "remote" </> ".git") do
      Process.callCommand "git init --bare"
    createDirectory (tempdir </> "local")
    createDirectory (tempdir </> "local" </> "main")
    withCurrentDirectory (tempdir </> "local" </> "main") do
      Process.callCommand "git init"
      Process.callCommand ("git remote add origin " ++ (tempdir </> "remote" </> ".git"))
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
