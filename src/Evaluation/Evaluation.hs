module Evaluation.Evaluation (evaluatePosition, evaluatePositionBreakdown, evaluateExchange, evaluateMvvLva)
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



evaluatePosition :: Position -> Score
evaluatePosition =
 evalScore . evaluatePositionBreakdown


evaluatePositionBreakdown :: Position -> ScoreBreakdown
evaluatePositionBreakdown pos =
  let
    !enemyPos         = makeNullMove pos
    !scoresBatch      = getScoresBatch pos
    !enemyScoresBatch = getScoresBatch enemyPos
    !playerBreakdown =
      evaluatePlayerBreakdown scoresBatch enemyScoresBatch pos
    !enemyBreakdown  =
      evaluatePlayerBreakdown enemyScoresBatch scoresBatch enemyPos
    !materialScore   =
         evalScore playerBreakdown.materialBreakdown
       - evalScore enemyBreakdown.materialBreakdown
    !materialTradesScore =
      evaluateMaterialTrades materialScore pos
  in
    ScoreBreakdown {..}
  where
    ?phase       = pos.phase
    ?colorToMove = pos.color


evaluatePlayerBreakdown :: (?phase :: Phase, ?colorToMove :: Color)
  => ScoresBatch -> ScoresBatch -> Position -> PlayerScoreBreakdown
evaluatePlayerBreakdown scoresBatch enemyScoresBatch pos =
  PlayerScoreBreakdown {
    materialBreakdown = evaluatePlayerMaterial pos pos.player pos.color
  , bonusBreakdown    = evaluatePositionBonuses scoresBatch pos
  , penaltyBreakdown  =
      evaluatePositionPenalties scoresBatch enemyScoresBatch pos
  }


evaluatePositionBonuses :: (?phase :: Phase, ?colorToMove :: Color)
  => ScoresBatch -> Position -> BonusBreakdown
evaluatePositionBonuses ScoresBatch {..} pos =
  BonusBreakdown {
    mobility           = mobility
  , passedPawns        = evaluatePassedPawns      pos
  , bishopPair         = evaluateBishopPair       pos
  , knightOutposts     = evaluateKnightOutposts   pos
  , rooksOnOpenFile    = evaluateRooksOnOpenFiles pos
  , kingPawnShield     = evaluateKingPawnShield   pos
  }


evaluatePositionPenalties ::
  ScoresBatch -> ScoresBatch -> Position -> PenaltyBreakdown
evaluatePositionPenalties
  ScoresBatch {threats} ScoresBatch {kingThreats} Position {player, pawns} =
  PenaltyBreakdown {
    threats       = threats
  , kingThreats   = kingThreats
  , isolatedPawns = evaluateIsolatedPawns (player & pawns)
  , doubledPawns  = evaluateDoubledPawns  (player & pawns)
  }


evaluateBishopPair :: (?phase :: Phase) => Position -> Score
evaluateBishopPair Position {player, bishops} =
   bishopPairBonus
   * fromIntegral (clamp (0, 1) (popCount (player & bishops) - 1))


evaluateKnightOutposts :: (?phase :: Phase) => Position -> Score
evaluateKnightOutposts Position {..} =
  knightOutpostBonus
  * fromIntegral (foldlBoard 0 (+) mapFn
    (knights&player & defended & ranks & knightOupostFiles))
  where
    defended = pawnAttacks color (player&pawns)
    mapFn !n = toReverseCondition (attackersVec !! n & enemy&pawns)
    (!ranks, !attackersVec) = case color of
      White -> (whiteKnightOutpostRanks , whiteKnightOutpostAttackersVec)
      Black -> (blackKnightOutpostRanks , blackKnightOutpostAttackersVec)


evaluateRooksOnOpenFiles :: (?phase :: Phase) => Position -> Score
evaluateRooksOnOpenFiles Position {..} =
  rookOnSemiOpenFileBonus
  * (eval file_A + eval file_B + eval file_C + eval file_D
   + eval file_E + eval file_F + eval file_G + eval file_H)
  where
    eval fileBoard
      | playerRooksInFile == 0                                          = 0
      | pawnsInFile       == 0 || lastPieceSquare pawnsInFile       <! n = 2
      | playerPawnsInFile == 0 || lastPieceSquare playerPawnsInFile <! n = 1
      | otherwise                                                      = 0
      where
        playerRooksInFile = player & rooks & fileBoard
        playerPawnsInFile = player & pawnsInFile
        pawnsInFile       = pawns  & fileBoard
        n                 = lastPieceSquare playerRooksInFile

    (!(<!), !lastPieceSquare) = case color of
      White -> ((<), msb)
      Black -> ((>), lsb)


evaluatePassedPawns ::
  (?phase :: Phase, ?colorToMove :: Color) => Position -> Score
evaluatePassedPawns pos@Position {..} =
  eval file_A + eval file_B + eval file_C + eval file_D
  + eval file_E + eval file_F + eval file_G + eval file_H
  where
    eval fileBoard
      | pawnsInFile == 0 || blockersVec !! n & enemy&pawns /= 0 = 0
      | otherwise = passerScore + escortedPasserScore
      where
        passerScore
          | isUnstoppablePawn n = unstoppablePawnBonus
          | isFreePasser rank n = freePassedPawnTable !!% rank
          | otherwise           = passedPawnTable     !!% rank
        escortedPasserScore = kingEscortedPassedPawnBonus *
           fromIntegral (getSquareDistance n enemyKingSquare
                       - getSquareDistance n kingSquare)
        rank            = normalizeRank $ toRank n
        pawnsInFile     = player & pawns & fileBoard
        n               = lastPawnSquare pawnsInFile
        kingSquare      = lsb (player & kings)
        enemyKingSquare = lsb (enemy & kings)

    isFreePasser rank n =
      testSquare noPieces (nextRank n)
      && evaluateExchange (Move Pawn promotion n $ nextRank n) pos >= 0
      where
        promotion | rank == 7 = QueenProm
                  | otherwise = NoProm

    isUnstoppablePawn pawnSquare =
        enemy & (knights .| bishops .| rooks .| queens) == 0
      && kingDistance > pawnDistance
      where
        pawnDistance = getSquareDistance pawnSquare promotionSquare
        kingDistance =
          getSquareDistance kingSquare promotionSquare - kingDistanceOffset

        promotionSquare = 8 * promotionRank + pawnFile
        pawnFile        = toFile pawnSquare
        kingSquare      = lsb (enemy & kings)
        kingDistanceOffset
          | color /= ?colorToMove = 1
          | otherwise            = 0

    (!normalizeRank, !nextRank, !lastPawnSquare, !blockersVec,
     !promotionRank) =
      case color of
        White ->
          (id   , (+ 8)     , msb, whitePassedPawnBlockersVec, 7)
        Black ->
          ((7 -), \n -> n - 8, lsb, blackPassedPawnBlockersVec, 0)
    !noPieces = (~) (player .| enemy)


evaluateKingPawnShield :: (?phase :: Phase) => Position -> Score
evaluateKingPawnShield Position {..} =
  maybe 0 go
    $ find ((`testSquare` kingSquare) . (kingRank &))
      [shortCastleFiles, longCastleFiles]
  where
    go board =
        pawnShield1RankBonus
        * popCountToScore (pawnShield1Rank & board & player & pawns)
      + pawnShield2RankBonus
        * popCountToScore (pawnShield2Rank & board & player & pawns)
    (kingRank, pawnShield1Rank, pawnShield2Rank) = case color of
      White -> (rank_1, rank_2, rank_3)
      Black -> (rank_8, rank_7, rank_6)
    kingSquare = lsb (player & kings)


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


evaluateDoubledPawns :: Board -> Score
evaluateDoubledPawns pawns =
  doubledPawnPenalty * fromIntegral doubledPawnsCount
  where
    doubledPawnsCount =
        max 1 (popCount (file_A & pawns))
      + max 1 (popCount (file_B & pawns))
      + max 1 (popCount (file_C & pawns))
      + max 1 (popCount (file_D & pawns))
      + max 1 (popCount (file_E & pawns))
      + max 1 (popCount (file_F & pawns))
      + max 1 (popCount (file_G & pawns))
      + max 1 (popCount (file_H & pawns))
      - 8


evaluateMaterialTrades :: Score -> Position -> Score
evaluateMaterialTrades materialScore Position {..} =
    convert (losingPenalty - winningPenalty)
  where
    losingPenalty =
      pieceTradesPenalty * losingMissingPieces * absoluteMaterialScore / 700
    winningPenalty =
      pawnTradesPenalty  * winningMissingPawns * absoluteMaterialScore / 800
    losingMissingPieces = max 0
      (7 - popCountToScore (losingBoard & (knights .| bishops .| rooks .| queens)))
    winningMissingPawns =
      8 - popCountToScore (winningBoard & pawns)
    absoluteMaterialScore = abs materialScore
    (winningBoard, losingBoard, convert)
      | materialScore > 0 = (player, enemy , id)
      | otherwise         = (enemy , player, negate)


getScoresBatch :: (?phase :: Phase) => Position -> ScoresBatch
getScoresBatch pos
  | isKingInCheck pos = emptyScoresBatch
getScoresBatch Position {..} = ScoresBatch {..}
  where
    mobility =
      knightsMobility + bishopsMobility + rooksMobility + queensMobility

    kingThreats =
      (kingThreatPiecesTable !! piecesCount) * kingThreatScore / 100

    piecesCount =
      knightsCount + bishopsCount + rooksCount + queensCount

    threats =
      queenThreat
        * popCountToScore (player & queens & (minorDefended .| pawnDefended))
      + rookThreat
        * popCountToScore (player & rooks & pawnDefended)
      + minorPieceThreat
        * popCountToScore (player & (knights .| bishops) & pawnDefended)

    kingThreatScore =
        threatByMinorPenalty * fromIntegral
          (byKnightThreats + byBishopThreats)
      + threatByRookPenalty  * fromIntegral byRookThreats
      + threatByQueenPenalty * fromIntegral byQueenThreats

    (knightsMobility, byKnightThreats, knightsCount) =
      foldBoardScores knightMobilityTable
        knightAttacks
        pawnDefended
        (unpinned&knights)

    (bishopsMobility, byBishopThreats, bishopsCount) =
      foldBoardScores bishopMobilityTable
        (bishopMoves (allPieces .\ player & queens) pinnedPieces king)
        pawnDefended
        (player&bishops)

    (rooksMobility, byRookThreats, rooksCount) =
      foldBoardScores rookMobilityTable
        (rookMoves (allPieces .\ player & queens)
         pinnedPieces king)
        (pawnDefended .| minorDefended)
        (player&rooks)

    (queensMobility, byQueenThreats, queensCount) =
      foldBoardScores queenMobilityTable
        (queenMoves allPieces pinnedPieces king)
        (pawnDefended .| minorDefended .| rookDefended)
        (player&queens)


    foldBoardScores !mobilityTable !movesFn !defended !board =
      foldlBoard (0, 0, 0) foldFn movesFn board
      where
        foldFn (!x, !y, !z) !attackArea =
          (x + mobilityTable
           !!% popCount (attackArea .\ (player .| defended)),
           y + threatenedSquares,
           z + toCondition threatenedSquares)
          where
            !threatenedSquares = popCount (enemyKingArea & attackArea)

    !king           = player & kings
    !unpinned       = player .\ pinnedPieces
    !allPieces      = player .| enemy

    !enemyKingArea  =
      kingAttacks (lsb (enemy&kings))
    !pawnDefended   =
      pawnAttacks (reverseColor color) (enemy&pawns)
    !minorDefended =
         foldBoardAttacks knightAttacks (enemy&knights)
      .| foldBoardAttacks (bishopAttacks allPieces) (enemy&bishops)
    !rookDefended =
       foldBoardAttacks (rookAttacks allPieces) (enemy&rooks)


evaluateExchange :: Move -> Position -> Score
evaluateExchange initialMv initialPos =
  let ?phase = initialPos.phase
  in go initialMv initialPos
  where
    !square = initialMv.end
    go !mv !pos =
      let newPos = makeMove mv pos
      in evaluateCapturedPiece mv pos
      - case headMay $ staticExchangeCaptures square newPos of
        Just newMv -> max 0 $! go newMv newPos
        Nothing    -> 0


{-# INLINE evaluateMvvLva #-}
evaluateMvvLva :: Move -> Position -> Word8
evaluateMvvLva Move {..} pos =
  promotionValue + maybe 0 exchangeValue (maybeCapturedPieceAt end pos)
  where
    exchangeValue capturedPiece =
      10 * (getPieceValue capturedPiece + 1)
      - getPieceValue piece
    promotionValue = 10 * (promotionPieceValue + 1)
    promotionPieceValue = case promotion of
      QueenProm  -> getPieceValue Queen
      KnightProm -> getPieceValue Knight
      BishopProm -> getPieceValue Bishop
      RookProm   -> getPieceValue Rook
      NoProm     -> 0


data ScoresBatch = ScoresBatch {
    mobility    :: Score
  , threats     :: Score
  , kingThreats :: Score

}

emptyScoresBatch :: ScoresBatch
emptyScoresBatch = ScoresBatch {
    mobility    = 0
  , threats     = 0
  , kingThreats = 0
}


popCountToScore :: Board -> Score
popCountToScore = fromIntegral . popCount
