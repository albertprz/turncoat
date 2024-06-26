module Search.Parameters where

import           AppPrelude            hiding ((/))
import           ClassyPrelude         ((/))

import qualified Data.Vector.Storable  as Vector
import           Evaluation.Parameters
import           Models.Score


initialAlpha :: Score
initialAlpha = minScore


initialBeta :: Score
initialBeta = maxScore


futilityMargins :: Vector Score
futilityMargins = Vector.fromList
  [2 * pawnScore, 4 * pawnScore, 8 * pawnScore]


getLmrDepth :: Int -> Depth -> Depth
getLmrDepth mvIdx depth =
    min (depth - 1)
    $ ceiling (lmrFactor * (fromIntegral depth / 2)
    + (1 - lmrFactor) * (fromIntegral depth - 1))
  where
    lmrFactor = min @Double 1 (fromIntegral mvIdx / 20)
