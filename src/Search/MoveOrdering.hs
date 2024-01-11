module Search.MoveOrdering where

import           AppPrelude                hiding (union, (\\))

import           Evaluation.StaticExchange
import           Models.Move
import           Models.Position
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.PieceCaptures     (allCaptures)
import           MoveGen.PieceQuietMoves   (allQuietMoves)


{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?tTable :: TTable) => Position -> IO [Move]
getSortedMoves !pos = do
  ttMove <- TTable.lookupBestMove $ getZobristKey pos
  pure (toList ttMove
            <> winningCaptures
            <> quietMoves
            <> losingCaptures)
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = allQuietMoves pos


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures !pos =
  bimapBoth (map fst)
  $ partition ((>= 0) . snd)
  $ sortOn (Down . snd)
  $ map attachEval captures
  where
    attachEval !mv = (mv, evaluateCaptureExchange pos mv)
    captures = allCaptures pos
