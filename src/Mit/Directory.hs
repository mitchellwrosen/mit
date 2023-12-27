module Mit.Directory
  ( cd,
    doesDirectoryExist,
  )
where

import Data.Text qualified as Text
import Mit.Prelude
import System.Directory qualified as Directory

-- | Change the working directory.
cd :: Text -> IO a -> IO a
cd dir =
  Directory.withCurrentDirectory (Text.unpack dir)

doesDirectoryExist :: Text -> IO Bool
doesDirectoryExist =
  Directory.doesDirectoryExist . Text.unpack
