module Utils.Maybe where

import           ClassyPrelude


{-# INLINE  maybeFilter #-}
maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter predicate ma = do
  a <- ma
  if predicate a
    then ma
    else Nothing


{-# INLINE  partitionTraverse #-}
partitionTraverse :: Monad m => (a -> m (Maybe b)) -> [a] -> m ([b], [a])
partitionTraverse = partitionTraverseHelper []


{-# INLINE  partitionTraverseHelper #-}
partitionTraverseHelper :: Monad m => [a] -> (a -> m (Maybe b)) -> [a] -> m ([b], [a])
partitionTraverseHelper acc f (x : xs) =
  do result <- f x
     (valid, nonValid) <- partitionTraverseHelper acc f xs
     pure $ maybe (valid, x : nonValid) ((,nonValid) . (: valid)) result

partitionTraverseHelper acc _ [] =
  pure ([], acc)



{-# INLINE  findTraverse #-}
findTraverse :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverse f (x : xs) = do
  result <- f x
  maybe (findTraverse f xs) (pure . Just) result

findTraverse _ [] =
  pure Nothing


{-# INLINE  iterateMaybe #-}
iterateMaybe :: (a -> Maybe a) -> a -> a
iterateMaybe f = go
  where
    go x = maybe x go (f x)
