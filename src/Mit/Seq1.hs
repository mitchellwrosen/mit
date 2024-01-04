module Mit.Seq1
  ( Seq1 (Cons, Snoc),
    Mit.Seq1.length,
    last,
    dropEnd,
    fromList,
    unsafeFromList,
    fromSeq,
    unsafeFromSeq,
    toSeq,
    toList,
    toList1,
  )
where

import Data.Coerce (coerce)
import Data.Foldable qualified
import Data.Foldable qualified as Foldable
import Data.Foldable1 (Foldable1 (foldMap1, foldMap1'))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Stack (HasCallStack)
import Prelude hiding (last)

newtype Seq1 a = Seq1 (Seq a)
  deriving newtype (Foldable)

pattern Cons :: a -> Seq a -> Seq1 a
pattern Cons x xs <- Seq1 (x Seq.:<| xs)

{-# COMPLETE Cons #-}

pattern Snoc :: Seq a -> a -> Seq1 a
pattern Snoc xs x <- Seq1 (xs Seq.:|> x)

{-# COMPLETE Snoc #-}

instance Foldable1 Seq1 where
  foldMap1 :: (Semigroup m) => (a -> m) -> Seq1 a -> m
  foldMap1 f (Cons x xs) =
    go (f x) (Foldable.toList xs)
    where
      go y = \case
        [] -> y
        z : zs -> y <> go (f z) zs

  foldMap1' :: (Semigroup m) => (a -> m) -> Seq1 a -> m
  foldMap1' f (Cons x xs) =
    Foldable.foldl' (\acc y -> acc <> f y) (f x) xs

length :: forall a. Seq1 a -> Int
length =
  coerce (Seq.length @a)

last :: Seq1 a -> a
last = \case
  Seq1 Seq.Empty -> undefined
  Seq1 (_ Seq.:|> x) -> x

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

fromList :: [a] -> Maybe (Seq1 a)
fromList = \case
  [] -> Nothing
  xs -> Just (Seq1 (Seq.fromList xs))

unsafeFromList :: (HasCallStack) => [a] -> Seq1 a
unsafeFromList =
  fromMaybe (error "unsafeFromList: empty list") . fromList

fromSeq :: Seq a -> Maybe (Seq1 a)
fromSeq = \case
  Seq.Empty -> Nothing
  xs -> Just (Seq1 xs)

unsafeFromSeq :: (HasCallStack) => Seq a -> Seq1 a
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
