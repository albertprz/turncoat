module Search.Perft where

import           AppPrelude

import           Models.Move        (Move)
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceMoves (allMoves)

import qualified Data.Map           as Map

{-# INLINE  perft #-}
perft :: Int -> Position -> Int
perft = go 0
  where
    go !nodes 0     _     = nodes + 1
    go !nodes !depth !pos = foldr f nodes (allMoves pos)
      where
        f mv acc = maybe acc
                         (go acc (depth - 1))
                         (playMove mv pos)


{-# INLINE  divide #-}
divide :: Int -> Position -> Map Move Int
divide 0     _   = Map.empty
divide depth pos = Map.fromList
  (second (perft (depth - 1)) <$> mapMaybe f (toList moves))
  where
    moves = allMoves pos
    f mv = (mv,) <$> playMove mv pos
