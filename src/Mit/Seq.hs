module Mit.Seq where

import qualified Data.Foldable as Foldable
import Data.Sequence

pattern NonEmpty :: Seq a
pattern NonEmpty <- _ :<| _

{-# COMPLETE Empty, NonEmpty #-}

pattern Singleton :: Seq a
pattern Singleton <- _ :<| Empty

toList :: Seq a -> [a]
toList =
  Foldable.toList
