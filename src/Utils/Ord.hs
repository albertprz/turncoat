module Utils.Ord where

import           ClassyPrelude


inRange :: Ord a => a -> a -> a -> Bool
inRange lo hi x = lo <= x && x <= hi

maximumBounded :: (MonoFoldable mono, Bounded (Element mono), Ord (Element mono)) => mono -> Element mono
maximumBounded = fromMaybe minBound . maximumMay


maximumByOrd :: (MonoFoldable mono, Ord b) =>
  (Element mono -> b) -> mono -> Maybe (Element mono)
maximumByOrd f =
  maximumByMay (compare `on` f)
