module Utils.Ord where

import           ClassyPrelude


{-# INLINE  inRange #-}
inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi x = lo <= x && x <= hi

{-# INLINE  andPred #-}
andPred :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andPred pred1 pred2 x =
  pred1 x && pred2 x
