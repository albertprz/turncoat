module Main where

import           AppPrelude

import           CommandLine.UciCommands (executeCommand)
import           Control.Monad.State     (evalStateT)
import           Models.Position         (startPosition)
import           Parsers.Command         (parseCommand)

import           Data.Char               (isSpace)


main :: IO ()
main = putStrLn "Apostate engine by albertprz" *>
       evalStateT (forever repl) startPosition
    where
    repl = do input <- getLine
              unless (all isSpace input)
                     (eval input)
    eval = either (putStrLn . (errorMsg <>) . tshow)
                  executeCommand
           . parseCommand
    errorMsg = "Error when parsing command: "
