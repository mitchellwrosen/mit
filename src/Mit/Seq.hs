module Mit.Seq where

import Data.Sequence

pattern NonEmpty :: Seq a
pattern NonEmpty <- _ :<| _

{-# COMPLETE Empty, NonEmpty #-}

pattern Singleton :: Seq a
pattern Singleton <- _ :<| Empty
