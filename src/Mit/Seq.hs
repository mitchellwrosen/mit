module Mit.Seq
  ( pattern NonEmpty,
    pattern Singleton,
    toList,
  )
where

import Data.Foldable qualified as Foldable
import Data.Sequence

pattern NonEmpty :: Seq a
pattern NonEmpty <- _ :<| _

{-# COMPLETE Empty, NonEmpty #-}

pattern Singleton :: Seq a
pattern Singleton <- _ :<| Empty

toList :: Seq a -> [a]
toList =
  Foldable.toList
