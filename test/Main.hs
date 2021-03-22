module Main where

import Control.Monad
import Control.Monad.Free.Church
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Mit hiding (main)
import System.Directory
import System.IO.Temp

main :: IO ()
main =
  when False do
    test
      "no remote branch"
      setupRemote0
      setupLocal0
      [ ("test 1", \() -> putStrLn "hi 1"),
        ("test 2", \() -> putStrLn "hi 2"),
        ("test 3", \() -> putStrLn "hi 3")
      ]

setupRemote0 :: F I ()
setupRemote0 = do
  icommit_ [("1.txt", ["1", "2", "3"]), ("2.txt", ["1", "2", "3"])]

setupLocal0 :: () -> F I ()
setupLocal0 () = do
  ionBranch "feature" do
    pure ()

test :: Text -> F I a -> (a -> F I b) -> [(Text, b -> IO ())] -> IO ()
test groupName setupRemote setupLocal actions =
  for_ actions \(testName, action) -> do
    tmpdir <- createTempDirectory "." "mit-test"
    createDirectoryIfMissing True (tmpdir ++ "/remote")
    setupRemoteResult <-
      withCurrentDirectory (tmpdir ++ "/remote") do
        git_ ["init", "--initial-branch=master"]
        run0 setupRemote
    withCurrentDirectory tmpdir (git_ ["clone", "remote", "local"])
    withCurrentDirectory (tmpdir ++ "/local") do
      Text.putStrLn ("[" <> groupName <> ", " <> testName <> "]")
      setupLocalResult <- run0 (setupLocal setupRemoteResult)
      action setupLocalResult
    removeDirectoryRecursive tmpdir
  where
    run0 :: F I a -> IO a
    run0 =
      iterM \case
        Commit files k -> do
          for_ files \(path, contents) -> Text.writeFile path (Text.unlines contents)
          git_ ["add", "--all"]
          git_ ["commit", "--message", "commit"]
          commit <- git ["rev-parse", "HEAD"]
          k commit
        OnBranch branch action k -> do
          exists <- git ["show-branch", branch]
          if exists then git_ ["switch", branch] else git_ ["switch", "--create", branch]
          run0 action >>= k

data I a
  = Commit [(FilePath, [Text])] (Text -> a)
  | forall x. OnBranch Text (F I x) (x -> a)

instance Functor I where
  fmap f = \case
    Commit a b -> Commit a (f . b)
    OnBranch a b c -> OnBranch a b (f . c)

icommit :: [(FilePath, [Text])] -> F I Text
icommit files =
  liftF (Commit files id)

icommit_ :: [(FilePath, [Text])] -> F I ()
icommit_ files =
  void (icommit files)

ionBranch :: Text -> F I a -> F I a
ionBranch branch action =
  liftF (OnBranch branch action id)
