{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluation.BoardScore where

import           AppPrelude

import           Constants.Boards
import           Evaluation.PieceSquareTables
import           Models.Move
import           Models.Piece
import           Models.Score


{-# INLINE  boardScore #-}
boardScore :: Score -> Vector Score -> Board -> Score
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard (Score 0) (+) pieceToScore board
  where
    pieceToScore !n = pieceTypeScore + pieceSquareTable !! n


{-# INLINE  evaluateCapturedPieceSquare #-}
evaluateCapturedPieceSquare ::  Color -> Square -> Piece -> Score
evaluateCapturedPieceSquare !color !n !piece =
  case piece of
    Pawn   -> pawnScore + pawnSquareTable !! idx
    Knight -> knightScore + knightSquareTable !! idx
    Bishop -> bishopScore + bishopSquareTable !! idx
    Rook   -> rookScore + rookSquareTable !! idx
    Queen  -> queenScore + queenSquareTable !! idx
  where
    idx = n + 64 * fromIntegral (reverseColor color)


{-# INLINE  evaluateCapturedPiece #-}
evaluateCapturedPiece :: Piece -> Score
evaluateCapturedPiece = \case
  Pawn -> pawnScore
  Knight -> knightScore
  Bishop -> bishopScore
  Rook -> rookScore
  Queen -> queenScore


{-# INLINE  evaluatePromotion #-}
evaluatePromotion :: Promotion -> Score
evaluatePromotion prom = score - pawnScore
  where
    score = case prom of
      KnightProm -> knightScore
      BishopProm -> bishopScore
      RookProm   -> rookScore
      QueenProm  -> queenScore


pawnScore :: Score
pawnScore = 100

knightScore :: Score
knightScore = 310

bishopScore :: Score
bishopScore = 320

rookScore :: Score
rookScore = 500

queenScore :: Score
queenScore = 900
