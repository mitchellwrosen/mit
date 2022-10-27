module Mit.Seq1
  ( Seq1,
    fromList,
    unsafeFromList,
    fromSeq,
    unsafeFromSeq,
    toSeq,
    toList,
    toList1,
    Mit.Seq1.length,
    dropEnd,
  )
where

import Data.Coerce
import Data.Foldable qualified
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Stack (HasCallStack)
import Prelude

newtype Seq1 a = Seq1 (Seq a)
  deriving newtype (Foldable)

fromList :: [a] -> Maybe (Seq1 a)
fromList = \case
  [] -> Nothing
  xs -> Just (Seq1 (Seq.fromList xs))

unsafeFromList :: HasCallStack => [a] -> Seq1 a
unsafeFromList =
  fromMaybe (error "unsafeFromList: empty list") . fromList

fromSeq :: Seq a -> Maybe (Seq1 a)
fromSeq = \case
  Seq.Empty -> Nothing
  xs -> Just (Seq1 xs)

unsafeFromSeq :: HasCallStack => Seq a -> Seq1 a
unsafeFromSeq =
  fromMaybe (error "unsafeFromSeq: empty sequence") . fromSeq

toSeq :: Seq1 a -> Seq a
toSeq =
  coerce

toList :: forall a. Seq1 a -> [a]
toList =
  coerce @(Seq a -> [a]) Data.Foldable.toList

toList1 :: Seq1 a -> List.NonEmpty a
toList1 =
  List.NonEmpty.fromList . Data.Foldable.toList

length :: forall a. Seq1 a -> Int
length =
  coerce (Seq.length @a)

dropEnd :: Int -> Seq1 a -> Seq a
dropEnd =
  let loop :: Int -> Seq a -> Seq a
      loop n =
        case n of
          0 -> id
          _ -> \case
            Seq.Empty -> Seq.Empty
            ys Seq.:|> _ -> loop (n - 1) ys
   in \n (Seq1 xs) -> loop n xs
