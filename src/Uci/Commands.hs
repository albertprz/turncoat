module Uci.Commands where

import           AppPrelude

import           Models.Position
import           Search.Perft

import           Data.Composition ((.:))
import           Data.Map         (traverseWithKey)


printPerft :: Int -> Position -> IO ()
printPerft = putStrLn . tshow .: perft


printDivide :: Int -> Position -> IO ()
printDivide = void
  . traverseWithKey (\k v -> putStrLn (tshow k <> " => " <> tshow v))
  .: divide
