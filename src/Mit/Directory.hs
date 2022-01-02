module Mit.Directory where

import qualified Data.Text as Text
import Mit.Prelude
import qualified System.Directory as Directory

doesDirectoryExist :: Text -> IO Bool
doesDirectoryExist =
  Directory.doesDirectoryExist . Text.unpack

withCurrentDirectory :: Text -> IO a -> IO a
withCurrentDirectory dir =
  Directory.withCurrentDirectory (Text.unpack dir)
