module AppPrelude (module ClassyPrelude, Vector, (!!)) where

import           ClassyPrelude        hiding (Vector, mask, (/), (>>), (^))

import           Data.Vector.Storable (Vector)

{-# INLINE  (!!) #-}
infixl 9 !!
(!!) :: Storable a => Vector a -> Int -> a
(!!) = unsafeIndex
