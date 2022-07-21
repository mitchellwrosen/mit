module Mit.Directory
  ( cd,
    doesDirectoryExist,
  )
where

import Data.Text qualified as Text
import Mit.Monad
import Mit.Prelude
import System.Directory qualified as Directory

cd :: Text -> Mit r x ()
cd dir =
  acquire_ (Directory.withCurrentDirectory (Text.unpack dir))

-- | Change directories (delimited by 'block').
doesDirectoryExist :: MonadIO m => Text -> m Bool
doesDirectoryExist =
  liftIO . Directory.doesDirectoryExist . Text.unpack
