module AppPrelude (module ClassyPrelude, Vector, (!!)) where

import           ClassyPrelude       hiding (Vector, mask, (/), (>>), (^))

import           Data.Vector.Unboxed (Vector, (!))

{-# INLINE  (!!) #-}
infixl 9 !!
(!!) :: Unbox a => Vector a -> Int -> a
(!!) = (!)
