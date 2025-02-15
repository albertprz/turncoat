module Search.Perft (perft, divide, allMoves) where

import           AppPrelude

import           Models.Move
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceCaptures
import           MoveGen.PieceQuietMoves

import qualified Data.Map                as Map
import           Models.Score


perft :: Depth -> Position -> Int
perft = go 0
  where
    go !nodes !depth !pos
      | depth == 1 = nodes + length moves
      | otherwise = foldl' f nodes moves
      where
        f !acc mv  = go acc (depth - 1) (makeMove mv pos)
        moves = allMoves pos


divide :: Depth -> Position -> Map Move Int
divide !depth !pos
  | depth == 1 = Map.fromList $ toList ((,1) <$> moves)
  | otherwise = Map.fromList
  (second (perft (depth - 1)) . getResults <$> toList moves)
  where
    getResults !mv = (mv, makeMove mv pos)
    moves = allMoves pos


allMoves :: Position -> [Move]
allMoves = allCaptures <> allQuietMoves
