module Utils.Maybe where

import           ClassyPrelude


{-# INLINE  maybeFilter #-}
maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter predicate ma = do
  a <- ma
  if predicate a then ma else Nothing
