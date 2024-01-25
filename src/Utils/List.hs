module Utils.List where

import           ClassyPrelude


{-# INLINE  splitAt1 #-}
splitAt1 :: Int -> [a] -> ([a], [a], [a], [a])
splitAt1 n xs = (ys, zs, [], [])
  where
    (ys, zs) = splitAt n xs


{-# INLINE  splitAt2 #-}
splitAt2 :: Int -> Int -> [a] -> ([a], [a], [a], [a])
splitAt2 n1 n2 xs = (ys, ts, us, [])
  where
    (ys, zs) = splitAt n1 xs
    (ts, us) = splitAt n2 zs


{-# INLINE  splitAt3 #-}
splitAt3 :: Int -> Int -> Int -> [a] -> ([a], [a], [a], [a])
splitAt3 n1 n2 n3 xs = (ys, ts, vs, ws)
  where
    (ys, zs) = splitAt n1 xs
    (ts, us) = splitAt n2 zs
    (vs, ws) = splitAt n3 us


{-# INLINE  tuple4Append #-}
tuple4Append :: ([a], [a], [a], [a]) -> [a]
tuple4Append (x, y, z, t) = x <> y <> z <> t
