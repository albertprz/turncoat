module Search.Perft where

import           AppPrelude

import           Models.Move        (Move)
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceMoves (allLegalMoves)

import qualified Data.Map           as Map
import qualified Data.Vector        as Vector

{-# INLINE  perft #-}
perft :: Int -> Position -> Int
perft = go 0
  where
    go !nodes  0      _   = nodes + 1
    go !nodes  1      pos = nodes + Vector.length (allLegalMoves pos)
    go !nodes !depth !pos = foldr f nodes (allLegalMoves pos)
      where
        f mv acc = go acc (depth - 1) (playMove mv pos)


{-# INLINE  divide #-}
divide :: Int -> Position -> Map Move Int
divide 0     _   = Map.empty
divide depth pos = Map.fromList
  (second (perft (depth - 1)) . getResults <$> toList (allLegalMoves pos))
  where
    getResults mv = (mv, playMove mv pos)
