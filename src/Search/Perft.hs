module Search.Perft where

import           AppPrelude

import           Models.Move        (Move)
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceMoves (allLegalMoves)

import qualified Data.Map           as Map
import           Models.Score

{-# INLINE  perft #-}
perft :: Depth -> Position -> Int
perft = go 0
  where
    go !nodes  1     !pos = nodes + length (allLegalMoves pos)
    go !nodes !depth !pos = foldr f nodes (allLegalMoves pos)
      where
        f mv acc = go acc (depth - 1) (makeMove mv pos)


{-# INLINE  divide #-}
divide :: Depth -> Position -> Map Move Int
divide (Depth 0)     _   = Map.empty
divide (Depth 1)     pos = Map.fromList $ toList ((,1) <$> allLegalMoves pos)
divide depth pos = Map.fromList
  (second (perft (depth - 1)) . getResults <$> toList (allLegalMoves pos))
  where
    getResults mv = (mv, makeMove mv pos)
