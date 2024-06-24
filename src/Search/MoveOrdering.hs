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
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  killerMoves <- getKillers ply pos
  let
    bestMoves =
         ttMove
      <> filter (`notElem` ttMove) winningCaptures

    worstMoves =
         filter (`notElem` ttMove) killerMoves
      <> filter (`notElem` (ttMove <> killerMoves)) quietMoves
      <> filter (`notElem` ttMove)                  losingCaptures

    !hasTTMove = not $ null ttMove

  pure if not (isKingInCheck pos) && depth >= 3
    then ((bestMoves, worstMoves)      , hasTTMove)
    else ((bestMoves <> worstMoves, []), hasTTMove)
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = getSortedQuietMoves depth pos


getKillers :: (?killersTable :: KillersTable)
  => Ply -> Position -> IO [Move]
getKillers !ply pos =
   filter (`isLegalQuietMove` pos)
   <$> KillersTable.lookupMoves ply


getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos =
  bimap mapFn mapFn
    $ partition ((>= 0) . snd)
    $ map attachEval
    $ filter (\x -> x.promotion `elem` [NoProm, QueenProm])
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
