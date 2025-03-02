module Mit.Git.GitCommitInfo
  ( GitCommitInfo (..),
    parseGitCommitInfo,
  )
where

import Data.Text qualified as Text
import Mit.Prelude

data GitCommitInfo = GitCommitInfo
  { author :: Text,
    date :: Text,
    hash :: Text,
    shorthash :: Text,
    subject :: Text
  }
  deriving stock (Show)

parseGitCommitInfo :: Text -> GitCommitInfo
parseGitCommitInfo line =
  case Text.split (== '\xFEFF') line of
    [author, date, hash, shorthash, subject] -> GitCommitInfo {author, date, hash, shorthash, subject}
    _ -> error (Text.unpack line)
