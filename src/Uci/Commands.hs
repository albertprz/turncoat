module Uci.Commands where

import           AppPrelude

import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.MakeMove
import           Search.Perft

import           Data.Composition ((.:))
import           Data.Map         (traverseWithKey)


printPerft :: Int -> Position -> IO ()
printPerft = putStrLn . tshow .: perft


printDivide :: Int -> Position -> IO ()
printDivide = void
  . traverseWithKey (\k v -> putStrLn (tshow k <> " => " <> tshow v))
  .: divide
