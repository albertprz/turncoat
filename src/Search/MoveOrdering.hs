module Search.MoveOrdering (getSortedMoves) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Command           (EngineOptions)
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove         (makeMove)
import           MoveGen.MoveQueries
import           MoveGen.PieceCaptures    (allCaptures)
import           MoveGen.PieceQuietMoves  (allQuietMoves)
import           MoveGen.PositionQueries
import qualified Utils.KillersTable       as KillersTable
import           Utils.KillersTable       (KillersTable)
import qualified Utils.TranspositionTable as TTable
import           Utils.TranspositionTable (TTable)



-- Move Ordering:
-- - Transposition table (PV / Refutation) move
-- - Winning captures (SEE >= 0) (Ordered by SEE)
-- - 2 Killer moves
-- - Quiet moves (Ordered by Static Eval)
-- - Losing captures (SEE < 0) (Ordered by SEE)

-- Reduced moves:
-- Late killers / quiet moves

getSortedMoves :: (?killersTable :: KillersTable, ?tTable :: TTable,
                  ?opts :: EngineOptions)
  => Depth -> Ply -> Position -> IO (([Move], [Move]), Bool)
getSortedMoves !depth !ply pos = do

  ttEntry     <- TTable.lookupEntry (getZobristKey pos)
  killerMoves <- getSortedKillers ply pos
  let
    ttMove    = toList ((.bestMove) =<< ttEntry)
    bestMoves =
         ttMove
      <> filter (`notElem` ttMove) winningCaptures
      <> filter (`notElem` ttMove) killerMoves

    worstMoves =
         filter (`notElem` (ttMove <> killerMoves)) quietMoves
      <> filter (`notElem` ttMove)                  losingCaptures

    !hasPVMove = any ((== PV) . (.nodeType)) ttEntry

  pure if depth >= 3 && not (isKingInCheck pos)
    then ((bestMoves, worstMoves)       , hasPVMove)
    else ((bestMoves <> worstMoves, []) , hasPVMove)
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = getSortedQuietMoves depth pos


getSortedKillers :: (?killersTable :: KillersTable)
  => Ply -> Position -> IO [Move]
getSortedKillers !ply pos =
  sortMoves pos
   . filter (`isLegalQuietMove` pos)
   <$> KillersTable.lookupMoves ply


getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos =
  bimap mapFn mapFn
    $ partition ((>= 0) . snd)
    $ map attachEval
    $ filter ((`member` bestPromotions) . (.promotion))
    $ allCaptures pos
  where
    !mapFn        = map fst . sortBy (comparing (Down . snd))
    attachEval mv = (mv, evaluateCaptureExchange mv pos)


getSortedQuietMoves :: Depth -> Position -> [Move]
getSortedQuietMoves depth pos
  | depth <= 2 = allQuietMoves pos
  | otherwise = sortMoves pos $ allQuietMoves pos


sortMoves :: Position -> [Move] -> [Move]
sortMoves pos = sortOn (Down . getMoveScore)
  where
    getMoveScore mv = - evaluatePosition (makeMove mv pos)
