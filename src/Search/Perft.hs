module Search.Perft where

import           AppPrelude

import           Models.Move        (Move)
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceMoves (allMoves)

import qualified Data.Map           as Map

perft :: Int -> Position -> Int
perft = go 0
  where
    go nodes 0     _   = nodes
    go nodes depth pos = foldl' f nodes (allMoves pos)
      where
        f acc mv =
          case playMove mv pos of
            Just pos' -> go (acc + 1) (depth - 1) pos'
            Nothing   -> acc


divide :: Int -> Position -> Map Move Int
divide 0     _   = Map.empty
divide depth pos = Map.fromList
  (second (perft (depth - 1)) <$> mapMaybe f moves)
  where
    moves = allMoves pos
    f mv = (mv,) <$> playMove mv pos
