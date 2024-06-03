module AppPrelude (module ClassyPrelude, Vector, (!!), inRange, maybeFilter, findTraverse, elem, notElem) where

import           ClassyPrelude        hiding (Vector, elem, mask, notElem, (/),
                                       (>>), (^))

import           Data.Foldable        (elem, notElem)
import           Data.Int             (Int16)
import           Data.Vector.Storable (Vector)


{-# SPECIALIZE (!!) :: Vector Int16 -> Int -> Int16 #-}
{-# SPECIALIZE (!!) :: Vector Word64 -> Int -> Word64 #-}
{-# INLINE (!!)  #-}
infixl 9 !!
(!!) :: Storable a => Vector a -> Int -> a
(!!) !x !y = unsafeIndex x y


{-# INLINE inRange  #-}
inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = lo <= x && x <= hi


{-# INLINE maybeFilter  #-}
maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter !predicate ma = do
  a <- ma
  if predicate a
    then ma
    else Nothing


{-# INLINE findTraverse  #-}
findTraverse :: Monad m => (Int -> a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverse !f = go 0
  where
    go !i (x : xs) = do
      result <- f i x
      maybe (go (i + 1) xs) (pure . Just) result
    go _ [] =
      pure Nothing
