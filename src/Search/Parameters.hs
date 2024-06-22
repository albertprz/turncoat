module Search.Parameters where

import           AppPrelude            hiding ((/))
import           ClassyPrelude         ((/))

import qualified Data.Vector.Storable  as Vector
import           Evaluation.Parameters
import           Models.Score


initialAlpha :: Score
initialAlpha = minBound + 1


initialBeta :: Score
initialBeta = maxBound - 1


futilityMargins :: Vector Score
futilityMargins = Vector.fromList
  [knightScore, rookScore, queenScore]


getLmrDepth :: Int -> Depth -> Depth
getLmrDepth !mvIdx !depth =
    min (depth - 1)
    $ ceiling (lmrFactor * (fromIntegral depth * 3 / 5)
    + (1 - lmrFactor) * (fromIntegral depth - 1))
  where
    !lmrFactor = min @Double 1 (fromIntegral mvIdx / 20)
