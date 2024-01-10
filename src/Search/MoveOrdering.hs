module Search.MoveOrdering where

import           AppPrelude                hiding (union, (\\))

import           Evaluation.StaticExchange
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
  pure (toList ttMove <> winCaptures <> allMoves)
  where
    (winCaptures, _) = getSortedCaptures pos
    allMoves = allLegalMoves pos


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures !pos =
  bimapBoth (map fst)
  $ partition ((> 0) . snd)
  $ sortOn (Down . snd)
  $ map addSee allCaptures
  where
    addSee !mv = (mv, evaluateCaptureExchange pos mv)
    allCaptures = allLegalCaptures pos
