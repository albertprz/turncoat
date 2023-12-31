module Utils.Ord where

import           ClassyPrelude


{-# INLINE  inRange #-}
inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi x = lo <= x && x <= hi
