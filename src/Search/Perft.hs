module Search.Perft where

import           AppPrelude

import           Models.Move
import           Models.Position
import           MoveGen.MakeMove

import qualified Data.Map           as Map
import           Models.Score
import           MoveGen.PieceMoves


{-# INLINE  perft #-}
perft :: Depth -> Position -> Int
perft = go 0
  where
    go !nodes !depth !pos
      | depth == 1 = nodes + length moves
      | otherwise = foldr f nodes moves
      where
        f !mv !acc = go acc (depth - 1) (makeMove mv pos)
        moves = allMoves pos


{-# INLINE  divide #-}
divide :: Depth -> Position -> Map Move Int
divide !depth !pos
  | depth == 0 = Map.empty
  | depth == 1 = Map.fromList $ toList ((,1) <$> moves)
  | otherwise = Map.fromList
  (second (perft (depth - 1)) . getResults <$> toList moves)
  where
    getResults !mv = (mv, makeMove mv pos)
    moves = allMoves pos
