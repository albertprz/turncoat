module Main where

import           ClassyPrelude

import           Uci.Commands
import           Models.Position 

main :: IO ()
main = printPerft 6 startPosition
