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
findTraverse :: Monad m => (Int -> a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverse = go 0
  where
    go !i f (x : xs) = do
      result <- f i x
      maybe (go (i + 1) f xs) (pure . Just) result
    go _ _ [] =
      pure Nothing
