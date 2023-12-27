module Main where

import           AppPrelude

import           CommandLine.Command (executeCommand, parseCommand)
import           Control.Monad.State (evalStateT)
import           Data.Char           (isSpace)
import           Models.Position     (startPosition)


main :: IO ()
main = putStrLn "Stafford engine by albertprz" *>
       evalStateT (forever repl) startPosition
    where
    repl = do input <- getLine
              unless (all isSpace input)
                     (eval input)
    eval = either (putStrLn . (errorMsg <>) . tshow)
                  executeCommand
           . parseCommand
    errorMsg = "Error when parsing command: "
