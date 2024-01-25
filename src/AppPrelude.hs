module AppPrelude (module ClassyPrelude, module Utils.Ord,
                   module Utils.Maybe, module Utils.Functor,
                   module Utils.List, Vector, (!!)) where

import           ClassyPrelude        hiding (Vector, mask, (/), (>>), (^))
import           Utils.Functor
import           Utils.List
import           Utils.Maybe
import           Utils.Ord

import           Data.Vector.Storable (Vector)


{-# INLINE  (!!) #-}
infixl 9 !!
(!!) :: Storable a => Vector a -> Int -> a
(!!) = unsafeIndex
