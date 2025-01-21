module Search.Quiescence (quiesceSearch) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.MoveQueries
import           MoveGen.PieceCaptures
import           MoveGen.PositionQueries
import           MoveGen.PieceQuietMoves
import           Search.Perft

import           Control.Monad.State


quiesceSearch :: (?nodes :: IORef Word64)
  => Score -> Score -> Ply -> Position -> IO Score
quiesceSearch !alpha !beta !ply !pos

  | standPat >= beta = pure beta

  | otherwise       = do
      modifyIORef' ?nodes (+ 1)
      (score, finalAlpha) <- runStateT scoreState newAlpha
      pure $! fromMaybe finalAlpha score
  where
    scoreState = findTraverseIndex (getMoveScore beta ply pos) moves
    moves
      | isKingInCheck pos = allMoves pos
      | ply <= 1           = getWinningCapturesAndChecks pos
      | otherwise         = getWinningCaptures pos
    !newAlpha  = max alpha standPat
    !standPat  = evaluatePosition pos


getMoveScore :: (?nodes :: IORef Word64)
  => Score -> Ply -> Position -> Int -> Move -> QuiesceM (Maybe Score)
getMoveScore !beta !ply !pos _ mv =
  do !alpha <- get
     !score <- liftIO (negate <$> quiesceSearch (-beta) (-alpha)
                                               (ply + 1)
                                               (makeMove mv pos))
     let !nodeType = getNodeType alpha beta score
     advanceState beta score nodeType


advanceState :: Score -> Score -> NodeType -> QuiesceM (Maybe Score)
advanceState !beta !score !nodeType =
  case nodeType of
    PV  -> Nothing <$ put score
    Cut -> pure $ Just beta
    All -> pure Nothing



getWinningCapturesAndChecks :: Position -> [Move]
getWinningCapturesAndChecks pos =
  getWinningCaptures pos
    <> filter (`isCheckMove` pos) (allQuietMoves pos)


getWinningCaptures :: Position -> [Move]
getWinningCaptures pos =
  filter (`isWinningCapture` pos) (allCaptures pos)



type QuiesceM = StateT Score IO
