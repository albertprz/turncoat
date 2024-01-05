module Search.MoveOrdering where

import           AppPrelude

import           Models.Move
import           Models.Position
import           Models.Score              (Depth)
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.PieceMoves


{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?tTable :: TTable) => Depth -> Position -> IO [Move]
getSortedMoves !depth !pos
  | depth == 1 = pure $ allLegalMoves pos
  | otherwise = do
    ttMove <- TTable.lookupBestMove $ getZobristKey pos
    pure $ case ttMove of
      Just mv -> mv : filter (/= mv) allMoves
      Nothing -> allMoves
    where
      allMoves = allLegalMoves pos
