module AppPrelude (module ClassyPrelude, Vector, (!!), inRange, maybeFilter, findTraverse) where

import           ClassyPrelude        hiding (Vector, mask, (/), (>>), (^))

import           Data.Vector.Storable (Vector)


{-# INLINE  (!!) #-}
infixl 9 !!
(!!) :: Storable a => Vector a -> Int -> a
(!!) = unsafeIndex

{-# INLINE  inRange #-}
inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = lo <= x && x <= hi

{-# INLINE  maybeFilter #-}
maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter predicate ma = do
  a <- ma
  if predicate a
    then ma
    else Nothing

{-# INLINE  findTraverse #-}
findTraverse :: Monad m => (Int -> a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverse = go 0
  where
    go !i f (x : xs) = do
      result <- f i x
      maybe (go (i + 1) f xs) (pure . Just) result
    go _ _ [] =
      pure Nothing
