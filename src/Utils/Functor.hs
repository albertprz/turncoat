module Utils.Functor where

import           ClassyPrelude


{-# INLINE  (<$$>) #-}
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f xs = map f <$> xs
