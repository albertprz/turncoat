module Evaluation.StaticExchange where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove      (makeMove)
import           MoveGen.PieceCaptures (staticExchangeCaptures)


{-# INLINE  evaluateCaptureExchange #-}
evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange mv@Move {..} pos =
  side * finalPos.materialScore - pos.materialScore
  where
    !side | finalPos.color == pos.color =   1
          | otherwise                  = - 1
    !finalPos = iterateMaybe (toQuietPosition end)
                             (makeMove mv pos)

{-# INLINE  toQuietPosition #-}
toQuietPosition :: Square -> Position -> Maybe Position
toQuietPosition square pos =
  (`makeMove` pos) <$> smallestAttackerMove
  where
    smallestAttackerMove =
      headMay $ staticExchangeCaptures square pos
