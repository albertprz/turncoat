module Main where

import           AppPrelude

import           CommandLine.Command (executeCommand, parseCommand)
import           Control.Monad.State (evalStateT)
import           Data.Char           (isSpace)
import           Models.Position     (startPosition)


main :: IO ()
main = evalStateT (forever repl) startPosition
  where
  repl = do
    putStrLn ""
    input <- getLine
    unless (all isSpace input)
      (eval input)
  eval input =
    either (putStrLn . ("Error when parsing command:" <>) . tshow)
           executeCommand
           (parseCommand input)
