module MoveGen.PieceMoves where

import           AppPrelude

import           Models.Move
import           Models.Position
import           MoveGen.PieceCaptures
import           MoveGen.PieceQuietMoves


{-# INLINE  allMoves #-}
allMoves :: Position -> [Move]
allMoves = allCaptures <> allQuietMoves
