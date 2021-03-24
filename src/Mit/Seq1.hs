module Mit.Seq1 where

import Data.Coerce
import qualified Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude

newtype Seq1 a = Seq1
  {unSeq1 :: Seq a}
  deriving newtype (Foldable)

dropEnd :: Int -> Seq1 a -> Seq a
dropEnd =
  let loop n =
        case n of
          0 -> id
          _ -> \case
            Seq.Empty -> Seq.Empty
            ys Seq.:|> _ -> loop (n -1) ys
   in \n (Seq1 xs) -> loop n xs

fromSeq :: Seq a -> Maybe (Seq1 a)
fromSeq = \case
  Seq.Empty -> Nothing
  xs -> Just (Seq1 xs)

length :: forall a. Seq1 a -> Int
length =
  coerce (Seq.length @a)

toList :: forall a. Seq1 a -> [a]
toList =
  coerce (Data.Foldable.toList @Seq @a)

toSeq :: Seq1 a -> Seq a
toSeq =
  coerce
