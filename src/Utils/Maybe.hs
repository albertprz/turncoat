module Utils.Maybe where

import           ClassyPrelude


{-# INLINE  maybeFilter #-}
maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter predicate ma = do
  a <- ma
  if predicate a
    then ma
    else Nothing


{-# INLINE  findTraverse #-}
findTraverse :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverse f (x : xs) = do
  result <- f x
  maybe (findTraverse f xs) (pure . Just) result

findTraverse _ [] =
  pure Nothing
