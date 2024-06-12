module AppPrelude (module ClassyPrelude, module Data.Ord, Vector, (!!), (/), (%), inRange, maybeFilter, findTraverseIndex, elem, notElem) where

import           ClassyPrelude        hiding (Vector, elem, mask, notElem, (/),
                                       (>>), (^))

import           Data.Foldable        (elem, notElem)
import           Data.Int             (Int16)
import           Data.Ord
import           Data.Vector.Storable (Vector)


{-# SPECIALIZE (!!) :: Vector Int16 -> Int -> Int16 #-}
{-# SPECIALIZE (!!) :: Vector Word64 -> Int -> Word64 #-}
{-# INLINE (!!)  #-}
infixl 9 !!
(!!) :: Storable a => Vector a -> Int -> a
(!!) !x !y = unsafeIndex x y


{-# SPECIALIZE (/) :: Int16 -> Int16 -> Int16 #-}
{-# SPECIALIZE (/) :: Int -> Int -> Int #-}
{-# INLINE (/)  #-}
infixl 7 /
(/) :: Integral a => a -> a -> a
(/) !x !y = quot x y

{-# SPECIALIZE (%) :: Int16 -> Int16 -> Int16 #-}
{-# SPECIALIZE (%) :: Int -> Int -> Int #-}
{-# SPECIALIZE (%) :: Word64 -> Word64 -> Word64 #-}
{-# INLINE (%)  #-}
infixl 7 %
(%) :: Integral a => a -> a -> a
(%) !x !y = rem x y


inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = lo <= x && x <= hi


{-# INLINE maybeFilter  #-}
maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter !predicate ma = do
  !a <- ma
  if predicate a
    then ma
    else Nothing


{-# INLINE findTraverseIndex  #-}
findTraverseIndex :: Monad m => (Int -> a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverseIndex !f = go 0
  where
    go !i (x : xs) = do
      result <- f i x
      maybe (go (i + 1) xs) (pure . Just) result
    go _ [] =
      pure Nothing
