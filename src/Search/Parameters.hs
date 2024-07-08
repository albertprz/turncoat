module Search.Parameters where

import           AppPrelude           hiding ((/))
import           ClassyPrelude        ((/))

import           Models.Score

import qualified Data.Vector.Storable as Vector


futilityMargins :: Vector Score
futilityMargins = Vector.fromList
  [300, 600, 1200]


getLmrDepth :: Int -> Depth -> Depth
getLmrDepth mvIdx depth =
  min (depth - 1) lmrDepth
  where
    lmrDepth = ceiling (lmrFactor * (fromIntegral depth / 2)
                + (1 - lmrFactor) * (fromIntegral depth - 1))
    lmrFactor = min @Double 1 (fromIntegral mvIdx / 80)


initialAlpha :: Score
initialAlpha = minScore


initialBeta :: Score
initialBeta = maxScore
