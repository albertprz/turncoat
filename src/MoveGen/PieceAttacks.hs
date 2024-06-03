module MoveGen.PieceAttacks (getLeapingCheckers, getSliderCheckers, getEnemyKingSliderRays, getBishopCheckerRays, getRookCheckerRays, getKingBishopRay, getPinnedPieces, getEnPassantPinnedPawns, getKingQueenRay, getKingRookRay, getAllAttacksAndScores, knightAttacks, bishopAttacks, rookAttacks, queenAttacks, kingAttacks, pawnAttacks, pawnDiagAttacks, pawnAntiDiagAttacks, filterPinnedAttacks) where

import           AppPrelude

import           Evaluation.Constants
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           Utils.Board



getLeapingCheckers :: Position -> Board
getLeapingCheckers Position {..} =
  player & checkers
  where
    checkers = pawns   & pawnAttacks   (reverseColor color) (enemy&kings)
            .| knights & knightAttacks (lsb (enemy&kings))


getSliderCheckers :: Board -> Board -> Position -> Board
getSliderCheckers bishopCheckerRays rookCheckerRays Position {..} =
  player & checkers
  where
    checkers = bishopCheckerRays & bishops
            .| rookCheckerRays   & rooks
            .| (bishopCheckerRays .| rookCheckerRays) & queens


getEnemyKingSliderRays :: Position -> Board
getEnemyKingSliderRays Position {..} =
  queenAttacks player (lsb (enemy&kings))


getBishopCheckerRays :: Position -> Board
getBishopCheckerRays Position {..} =
  bishopAttacks (enemy .| player) (lsb (enemy&kings))


getRookCheckerRays :: Position -> Board
getRookCheckerRays Position {..} =
  rookAttacks (enemy .| player) (lsb (enemy&kings))


getPinnedPieces :: Board -> Board -> Board -> Position -> Board
getPinnedPieces !bishopCheckerRays !rookCheckerRays !sliderRays Position {..} =
  enemy &
  (foldBoard bishopPins (attackers & bishops)
  .| foldBoard rookPins (attackers & rooks)
  .| foldBoard queenPins (attackers & queens))
  where
    bishopPins n = getKingBishopRay king n
      & bishopCheckerRays
      & bishopAttacks allPieces n
    rookPins n = getKingRookRay king n
      & rookCheckerRays
      & rookAttacks allPieces n
    queenPins n = getKingQueenRay king n
      & (bishopCheckerRays .| rookCheckerRays)
      & queenAttacks allPieces n
    attackers = sliderRays & player
    !king = enemy & kings
    allPieces = player .| enemy


getEnPassantPinnedPawns :: Position -> Board
getEnPassantPinnedPawns pos@Position {..} =
  pawns & getPinnedPieces 0 rookCheckerRays sliderRays pos'
  where
    rookCheckerRays = getRookCheckerRays pos'
    sliderRays = getEnemyKingSliderRays pos' & enPassantRank
    pos' = pos {
      pawns = pawns ^ enPassantPawn,
      player = player ^ enPassantPawn
    }
    enPassantPawn = case color of
      White -> enPassant << 8
      Black -> enPassant >> 8
    enPassantSquare = lsb enPassantPawn
    enPassantRank = fileMovesVec !! enPassantSquare


getKingQueenRay :: Board -> Square -> Board
getKingQueenRay !king !n
  | file & king /= 0     = file
  | rank & king /= 0     = rank
  | diag & king /= 0     = diag
  | antiDiag & king /= 0 = antiDiag
  | otherwise           = 0
  where
    file = rankMovesVec !! n
    rank = fileMovesVec !! n
    diag = antiDiagMovesVec !! n
    antiDiag = diagMovesVec !! n


getKingBishopRay :: Board -> Square -> Board
getKingBishopRay !king !n
  | diag & king /= 0     = diag
  | antiDiag & king /= 0 = antiDiag
  | otherwise           = 0
  where
    diag = antiDiagMovesVec !! n
    antiDiag = diagMovesVec !! n


getKingRookRay :: Board -> Square -> Board
getKingRookRay !king !n
  | file & king /= 0 = file
  | rank & king /= 0 = rank
  | otherwise       = 0
  where
    file = rankMovesVec !! n
    rank = fileMovesVec !! n


getAllAttacksAndScores :: Position -> (Board, Score, Score)
getAllAttacksAndScores Position {..} =
  (allAttacks, mobility, unsafe)
  where
    !allAttacks   = pawnThreats .| knightThreats .| bishopThreats
                 .| rookThreats .| queenThreats .| kingThreats
    !mobility     = knightsScore + bishopsScore + rooksScore + queensScore
    !unsafe       = (kingZonePenalty !! unsafePieces) * unsafeScore `div` 100
    !unsafePieces =
      knightsUnsafe + bishopsUnsafe + rooksUnsafe + queensUnsafe
    !unsafeScore  =
      minorSafetyPenalty *
        fromIntegral (knightsUnsafeSquares + bishopsUnsafeSquares)
      + rookSafetyPenalty * fromIntegral rooksUnsafeSquares
      + queenSafetyPenalty * fromIntegral queensUnsafeSquares

    (knightThreats, knightsScore, knightsUnsafeSquares, knightsUnsafe) =
      foldBoardMobility knightMobilityTable
      ((.\ player) . knightAttacks) (player&knights)

    (bishopThreats, bishopsScore, bishopsUnsafeSquares, bishopsUnsafe) =
      foldBoardMobility bishopMobilityTable
      ((.\ player) . bishopAttacks allPieces) (player&bishops)

    (rookThreats, rooksScore, rooksUnsafeSquares, rooksUnsafe) =
      foldBoardMobility rookMobilityTable
      ((.\ player) . rookAttacks allPieces) (player&rooks)

    (queenThreats, queensScore, queensUnsafeSquares, queensUnsafe) =
      foldBoardMobility queenMobilityTable
      ((.\ player) . queenAttacks allPieces) (player&queens)

    !pawnThreats   = pawnAttacks color (player&pawns)
    !kingThreats   = kingAttacks (lsb (player&kings))
    !allPieces     = player .| (enemy .\ kings)
    !enemyKingArea = kingAttacks (lsb (enemy&kings))

    foldBoardMobility !table !f !board =
      foldlBoard (0, 0, 0, 0) foldFn f board
      where
        foldFn (!x, !y, !z, !t) !curr =
          (x .| curr,
           y + table !! ones curr,
           z + unsafeSquares,
           t + min 1 unsafeSquares
          )
          where
            !unsafeSquares = ones (enemyKingArea & curr)



pawnAttacks :: Color -> Board -> Board
pawnAttacks color !board = case color of
  White -> (board .\ file_A) << 7 .| (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9 .| (board .\ file_H) >> 7


pawnDiagAttacks :: Color -> Board -> Board
pawnDiagAttacks color !board = case color of
  White -> (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9


pawnAntiDiagAttacks :: Color -> Board -> Board
pawnAntiDiagAttacks color !board = case color of
  White -> (board .\ file_A) << 7
  Black -> (board .\ file_H) >> 7


knightAttacks :: Square -> Board
knightAttacks !n = knightMovesVec !! n


kingAttacks :: Square -> Board
kingAttacks !n = kingMovesVec !! n


filterPinnedAttacks :: Board -> Board -> Board -> Square -> Board
filterPinnedAttacks pinnedPieces attacks ray n
  | testSquare pinnedPieces n = attacks & ray
  | otherwise                 = attacks


bishopAttacks :: Board -> Square -> Board
bishopAttacks !allPieces !n =
     sliding lsb northEastMovesVec allPieces n
  .| sliding lsb northWestMovesVec allPieces n
  .| sliding msb southWestMovesVec allPieces n
  .| sliding msb southEastMovesVec allPieces n


rookAttacks :: Board -> Square -> Board
rookAttacks !allPieces !n =
     sliding lsb northMovesVec allPieces n
  .| sliding lsb eastMovesVec allPieces n
  .| sliding msb westMovesVec allPieces n
  .| sliding msb southMovesVec allPieces n


queenAttacks :: Board -> Square -> Board
queenAttacks !allPieces !n =
     rookAttacks allPieces n
  .| bishopAttacks allPieces n


sliding :: (Board -> Square) -> Vector Board -> Board -> Square -> Board
sliding !findBlocker !mask !allPieces !n =
   ray ^ blockerRay
  where
    !blockerRay = mask !! firstBlocker
    !firstBlocker = findBlocker blockers
    !blockers = ray & allPieces
    !ray = mask !! n
