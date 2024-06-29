module Search.Parameters where

import           AppPrelude           hiding ((/))
import           ClassyPrelude        ((/))

import qualified Data.Vector.Storable as Vector
import           Models.Score


initialAlpha :: Score
initialAlpha = minScore


initialBeta :: Score
initialBeta = maxScore

futilityMargins :: Vector Score
futilityMargins = Vector.fromList
  [300, 600, 1000]


getLmrDepth :: Int -> Depth -> Depth
getLmrDepth mvIdx depth =
    min (depth - 1)
    $ ceiling (lmrFactor * (fromIntegral depth / 2)
    + (1 - lmrFactor) * (fromIntegral depth - 1))
  where
    lmrFactor = min @Double 1 (fromIntegral mvIdx / 60)
