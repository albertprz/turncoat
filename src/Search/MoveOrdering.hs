module Search.MoveOrdering where

import           AppPrelude                hiding (intersect, (\\))

import           Evaluation.StaticExchange
import           Models.KillersTable       (KillersTable)
import qualified Models.KillersTable       as KillersTable
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove          (makeMove)
import           MoveGen.MoveQueries       (isKingInCheck, isLegalQuietMove)
import           MoveGen.PieceCaptures     (allCaptures)
import           MoveGen.PieceQuietMoves   (allQuietMoves)



{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?killersTable :: KillersTable, ?tTable::TTable)
  => Depth -> Ply -> Position -> IO ([Move], [Move])
getSortedMoves !depth !ply pos = do
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  killerMoves <- filter (andPred (`notElem` ttMove)
                                (`isLegalQuietMove` pos))
                  <$> KillersTable.lookupMoves ply
  let
    bestMoves = ttMove
      <> filter (`notElem` ttMove) winningCaptures
    worstMoves =
      killerMoves
      <> filter (`notElem` (ttMove <> killerMoves)) quietMoves
      <> filter (`notElem` ttMove)                  losingCaptures
  if depth >= 2 && not (isKingInCheck pos) then
    pure (first (bestMoves <>) $ splitAt 4 worstMoves)
  else
    pure (bestMoves <> worstMoves, [])
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
    quietMoves      = allQuietMoves pos
