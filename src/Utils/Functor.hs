module Utils.Functor where

import           ClassyPrelude


{-# INLINE  (<$$>) #-}
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f xs = map f <$> xs

{-# INLINE  bimapBoth #-}
bimapBoth :: Bifunctor p => (a -> b) -> p a a -> p b b
bimapBoth f = bimap f f

{-# INLINE  first3 #-}
first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f (x, y, z) = (f x, y, z)

{-# INLINE  second3 #-}
second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f (x, y, z) = (x, f y, z)

{-# INLINE  third3 #-}
third3 :: (c -> c') -> (a, b, c) -> (a, b, c')
third3 f (x, y, z) = (x, y, f z)


{-# INLINE  fst3 #-}
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

{-# INLINE  fst4 #-}
fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x
