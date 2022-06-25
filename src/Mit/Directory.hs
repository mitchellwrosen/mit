module Mit.Directory
  ( doesDirectoryExist,
    withCurrentDirectory,
  )
where

import Data.Text qualified as Text
import Mit.Prelude
import System.Directory qualified as Directory

doesDirectoryExist :: Text -> IO Bool
doesDirectoryExist =
  Directory.doesDirectoryExist . Text.unpack

withCurrentDirectory :: Text -> IO a -> IO a
withCurrentDirectory dir =
  Directory.withCurrentDirectory (Text.unpack dir)
