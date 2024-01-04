-- | Random text-related utilities.
module Mit.TextUtils
  ( commaSeparated,
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as Text.Builder
import Mit.Prelude

-- >>> commaSeparated (Seq.fromList ["foo", "bar", "baz"])
-- "foo,bar,baz"
commaSeparated :: Seq Text -> Text
commaSeparated = \case
  Seq.Empty -> Text.empty
  xs Seq.:|> x ->
    Text.Builder.runBuilder $
      foldr (\y ys -> Text.Builder.fromText y <> Text.Builder.fromChar ',' <> ys) (Text.Builder.fromText x) xs
