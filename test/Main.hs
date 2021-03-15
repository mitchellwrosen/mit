module Main where

-- import Control.Exception (IOException, catch)
import Control.Monad.Free.Church
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Foldable
import qualified Data.Text.IO as Text
import Mit hiding (main)
import System.Directory
import System.IO.Temp

main :: IO ()
main =
  run setup

run :: F I a -> IO a
run =
  iterM \case
    Gitcommit files k -> do
      for_ files \(path, contents) -> Text.writeFile path (Text.unlines contents)
      git_ ["add", "--all"]
      git_ ["commit", "--message", "commit"]
      commit <- git ["rev-parse", "HEAD"]
      k commit
    Gitinit k -> do
      git_ ["init", "--initial-branch=master"]
      k
    Indir path program k ->
      withCurrentDirectory path (run program) >>= k
    Mkdir path k -> do
      createDirectoryIfMissing True path
      k
    Rmdir path k -> do
      removeDirectoryRecursive path
      k
    Tmpdir k -> createTempDirectory "." "mit-tests" >>= k

data I a
  = Gitcommit [(FilePath, [Text])] (Text -> a)
  | Gitinit a
  | forall x. Indir FilePath (F I x) (x -> a)
  | Mkdir FilePath a
  | Rmdir FilePath a
  | Tmpdir (FilePath -> a)

instance Functor I where
  fmap f = \case
    Gitcommit a b -> Gitcommit a (f . b)
    Gitinit a -> Gitinit (f a)
    Indir a b c -> Indir a b (f . c)
    Mkdir a b -> Mkdir a (f b)
    Rmdir a b -> Rmdir a (f b)
    Tmpdir a -> Tmpdir (f . a)

igitcommit :: [(FilePath, [Text])] -> F I Text
igitcommit files =
  liftF (Gitcommit files id)

igitcommit_ :: [(FilePath, [Text])] -> F I ()
igitcommit_ files =
  void (igitcommit files)

igitinit :: F I ()
igitinit =
  liftF (Gitinit ())

iindir :: FilePath -> F I a -> F I a
iindir path program =
  liftF (Indir path program id)

imkdir :: FilePath -> F I ()
imkdir path =
  liftF (Mkdir path ())

irmdir :: FilePath -> F I ()
irmdir path =
  liftF (Rmdir path ())

itmpdir :: F I FilePath
itmpdir =
  liftF (Tmpdir id)

setup :: F I ()
setup = do
  dir <- itmpdir
  imkdir (dir ++ "/local")
  iindir (dir ++ "/local") do
    igitinit
    igitcommit_ [("1.txt", ["1", "2", "3"]), ("2.txt", ["1", "2", "3"])]
  irmdir dir
