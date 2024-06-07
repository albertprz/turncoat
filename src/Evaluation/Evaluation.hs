module Evaluation.Evaluation (evaluateCaptureExchange, evaluatePosition,
getScoreBreakdown)
where

import           AppPrelude

import           Evaluation.Material
import           Evaluation.Parameters
import           Evaluation.ScoreBreakdown
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.PieceAttacks
import           MoveGen.PieceCaptures
import           MoveGen.PositionQueries
import           Utils.Board


getScoreBreakdown :: Position -> ScoreBreakdown
getScoreBreakdown pos@Position{..} = ScoreBreakdown {..}
  where
    (white, black)
      | color == White = (playerBreakdown, enemyBreakdown)
      | otherwise     = (enemyBreakdown, playerBreakdown)
    playerBreakdown =
      (evaluatePlayerBreakdown scoresBatch enemyScoresBatch pos)
      { material = Just $ evaluatePlayerMaterial pos player color }
    enemyBreakdown  =
      (evaluatePlayerBreakdown enemyScoresBatch scoresBatch enemyPos)
      { material = Just $ evaluatePlayerMaterial pos enemy enemyColor }
    enemyPos         = makeNullMove pos
    enemyColor       = reverseColor color
    scoresBatch      = getScoresBatch pos
    enemyScoresBatch = getScoresBatch enemyPos


evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange initialMv initialPos =
  evaluateExchange initialMv.end (makeMove initialMv initialPos)
    - initialPos.materialScore
  where
    evaluateExchange !square pos =
      case headMay $ staticExchangeCaptures square pos of
        Just mv -> - (max pos.materialScore
                        $! evaluateExchange square (makeMove mv pos))
        Nothing -> - pos.materialScore


evaluatePosition :: Position -> Score
evaluatePosition pos =
  pos.materialScore
  + evalScore
    (evaluatePlayerBreakdown scoresBatch enemyScoresBatch pos)
  - evalScore
    (evaluatePlayerBreakdown enemyScoresBatch scoresBatch  enemyPos)
  where
    enemyPos         = makeNullMove pos
    scoresBatch      = getScoresBatch pos
    enemyScoresBatch = getScoresBatch enemyPos


evaluatePlayerBreakdown
  :: ScoresBatch -> ScoresBatch -> Position -> PlayerScoreBreakdown
evaluatePlayerBreakdown scoresBatch enemyScoresBatch pos =
  PlayerScoreBreakdown {
    material      = Nothing
  , bonusScores   = evaluatePositionBonuses   scoresBatch      pos
  , penaltyScores = evaluatePositionPenalties enemyScoresBatch pos
  }


evaluatePositionBonuses :: ScoresBatch -> Position -> BonusBreakdown
evaluatePositionBonuses ScoresBatch {mobility} pos =
  BonusBreakdown {
    mobility       = mobility
  , bishopPair     = evaluateBishopPair pos
  , knightOutposts = evaluateKnightOutposts pos
  , passedPawns    = evaluatePassedPawns pos
  }

evaluatePositionPenalties :: ScoresBatch -> Position -> PenaltyBreakdown
evaluatePositionPenalties
  ScoresBatch {kingThreats} Position {player, pawns} =
  PenaltyBreakdown {
    kingThreats   = kingThreats
  , isolatedPawns = evaluateIsolatedPawns (player & pawns)
  }


evaluateBishopPair :: Position -> Score
evaluateBishopPair Position {player, bishops} =
   bishopPairBonus
   * fromIntegral (clamp (0, 1) (popCount (player & bishops) - 1))


evaluateIsolatedPawns :: Board -> Score
evaluateIsolatedPawns pawns =
  isolatedPawnPenalty * fromIntegral isolatedPawnsCount
  where
    isolatedPawnsCount =
      popCountToBoard (file_B & pawns)
        * toReverseCondition (pawns & (file_A .| file_C))
      + popCountToBoard (file_C & pawns)
        * toReverseCondition (pawns & (file_B .| file_D))
      + popCountToBoard (file_D & pawns)
        * toReverseCondition (pawns & (file_C .| file_E))
      + popCountToBoard (file_E & pawns)
        * toReverseCondition (pawns & (file_D .| file_F))
      + popCountToBoard (file_F & pawns)
        * toReverseCondition (pawns & (file_E .| file_G))
      + popCountToBoard (file_G & pawns)
        * toReverseCondition (pawns & (file_F .| file_H))


evaluateKnightOutposts :: Position -> Score
evaluateKnightOutposts Position {..} =
    knightOutpostBonus
  * fromIntegral (foldlBoard 0 (+) mapFn
    (knights&player & defended & ranks & knightOupostFiles))
  where
    defended = pawnAttacks color (player&pawns)
    mapFn !n = toReverseCondition (attackersVec !! n & enemy&pawns)
    (!ranks, !attackersVec)
      | color == White =
        (whiteKnightOutpostRanks , whiteKnightOutpostAttackersVec)
      | otherwise =
         (blackKnightOutpostRanks, blackKnightOutpostAttackersVec)


evaluatePassedPawns :: Position -> Score
evaluatePassedPawns Position {..} =
  eval file_A + eval file_B + eval file_C + eval file_D
  + eval file_E + eval file_F + eval file_G + eval file_H
  where
    eval board = mapFn $ msb (player & pawns & board)
    mapFn n
      | n == 64 || blockersVec !! n & enemy&pawns /= 0 = 0
      | otherwise = passedPawnTable !! getRank n
    (!getRank, !blockersVec)
      | color == White = (toRank           , whitePassedPawnBlockersVec)
      | otherwise     = (\n -> 7 - toRank n, blackPassedPawnBlockersVec)


getScoresBatch :: Position -> ScoresBatch
getScoresBatch pos
  | isKingInCheck pos = emptyScoresBatch

getScoresBatch Position {..} = ScoresBatch {..}
  where
    mobility        =
        knightsMobility + bishopsMobility + rooksMobility + queensMobility

    kingThreats     =
      (kingThreatPiecesTable !! piecesCount) * kingThreatScore `div` 100

    piecesCount     =
      knightsCount + bishopsCount + rooksCount + queensCount

    kingThreatScore =
        threatByMinorPenalty * fromIntegral
          (byKnightThreats + byBishopThreats)
      + threatByRookPenalty  * fromIntegral byRookThreats
      + threatByQueenPenalty * fromIntegral byQueenThreats

    (knightsMobility, byKnightThreats, knightsCount) =
      foldBoardScores knightMobilityTable
        knightAttacks
        (unpinned&knights)

    (bishopsMobility, byBishopThreats, bishopsCount) =
      foldBoardScores bishopMobilityTable
        (bishopMoves allPieces pinnedPieces king)
        (player&bishops)

    (rooksMobility, byRookThreats, rooksCount)       =
      foldBoardScores rookMobilityTable
        (rookMoves allPieces pinnedPieces king)
        (player&rooks)

    (queensMobility, byQueenThreats, queensCount)    =
      foldBoardScores queenMobilityTable
        (queenMoves allPieces pinnedPieces king)
        (player&queens)


    foldBoardScores !mobilityTable !f !board =
      foldlBoard (0, 0, 0) foldFn f board
      where
        foldFn (!x, !y, !z) !attackArea =
          (x + mobilityTable !! popCount (attackArea .\ player),
           y + threatenedSquares,
           z + toCondition threatenedSquares)
          where
            !threatenedSquares = popCount (enemyKingArea & attackArea)

    !king          = player & kings
    !unpinned      = player .\ pinnedPieces
    !allPieces     = player .| enemy
    !enemyKingArea = kingAttacks (lsb (enemy&kings))


data ScoresBatch = ScoresBatch {
  mobility    :: Score,
  kingThreats :: Score
}

emptyScoresBatch :: ScoresBatch
emptyScoresBatch = ScoresBatch {
    mobility = 0
  , kingThreats = 0
}
