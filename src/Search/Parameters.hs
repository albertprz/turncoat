module Search.Parameters where

import           AppPrelude           hiding ((/))
import           ClassyPrelude        ((/))

import qualified Data.Vector.Storable as Vector
import           Models.Score


futilityMargins :: Vector Score
futilityMargins = Vector.fromList
  [150, 300, 600]


getLmrDepth :: Int -> Depth -> Depth
getLmrDepth mvIdx depth =
  min (depth - 1) lmrDepth
  where
    lmrDepth = ceiling (lmrFactor * (fromIntegral depth / 2)
                + (1 - lmrFactor) * (fromIntegral depth - 1))
    lmrFactor = min @Double 1 (fromIntegral mvIdx / 40)


initialAlpha :: Score
initialAlpha = minScore


initialBeta :: Score
initialBeta = maxScore
