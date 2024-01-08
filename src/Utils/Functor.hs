module Utils.Functor where

import           ClassyPrelude


{-# INLINE  (<$$>) #-}
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f xs = map f <$> xs

{-# INLINE  fst3 #-}
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

{-# INLINE  fst4 #-}
fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a
