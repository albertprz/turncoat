module Search.MoveOrdering where

import           AppPrelude

import           Models.Move
import           Models.Position
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.PieceCaptures     (allLegalCaptures)
import           MoveGen.PieceMoves        (allLegalMoves)


{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?tTable :: TTable) => Position -> IO [Move]
getSortedMoves !pos = do
    ttMove <- TTable.lookupBestMove $ getZobristKey pos
    pure $ case ttMove of
      Just mv -> mv : filter (/= mv) allMoves
      Nothing -> allMoves
    where
      allMoves = allLegalMoves pos

{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> [Move]
getSortedCaptures !pos = allMoves
  where
    allMoves = allLegalCaptures pos
