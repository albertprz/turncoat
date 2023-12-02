module Main where

import           ClassyPrelude

import           Models.Position (startPosition)
import           Uci.Commands


main :: IO ()
main = printPerft 6 startPosition
