module Search.MoveOrdering where

import           AppPrelude                hiding (union, (\\))

import           Evaluation.StaticExchange
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove          (makeMove)
import           MoveGen.PieceAttacks      (isKingInCheck)
import           MoveGen.PieceCaptures     (allCaptures)
import           MoveGen.PieceQuietMoves   (allQuietMoves)


{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?tTable :: TTable) => Depth -> Position -> IO ([Move], [Move])
getSortedMoves depth pos = do
  ttMove <- TTable.lookupBestMove $ getZobristKey pos
  let
    bestMoves = toList ttMove <> winningCaptures
    worstMoves = quietMoves <> losingCaptures
    mainMoves = bestMoves <> take 3 worstMoves
    reducedMoves = drop 3 worstMoves
  if depth < 3 || isKingInCheck pos then
    pure (mainMoves <> reducedMoves, [])
  else
    pure (mainMoves, reducedMoves)
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = getSortedQuietMoves pos


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos =
  bimapBoth (map fst)
  $ partition ((>= 0) . snd)
  $ sortOn (Down . snd)
  $ map attachEval captures
  where
    attachEval mv = (mv, evaluateCaptureExchange mv pos)
    captures = allCaptures pos

{-# INLINE  getSortedQuietMoves #-}
getSortedQuietMoves :: Position -> [Move]
getSortedQuietMoves pos =
  sortOn (Down . getMoveScore) quietMoves
  where
    getMoveScore mv = - (makeMove mv pos).materialScore
    quietMoves       = allQuietMoves pos
