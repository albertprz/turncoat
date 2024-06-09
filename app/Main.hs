module Main where

import           AppPrelude

import           CommandLine.UciCommands (executeCommand)
import           Control.Monad.State     (evalStateT)
import           Models.Command          
import           Parsers.Command         (parseCommand)

import           Data.Char               (isSpace)


main :: IO ()
main = do
  printHeader
  evalStateT (forever repl) initialEngineState
  where
    repl = do input <- getLine
              unless (all isSpace input)
                     (eval input)
    eval = either (const $ putStrLn parseErrorMsg) executeCommand
           . parseCommand


printHeader :: IO ()
printHeader =
  putStrLn (name <> " " <> version <> " by " <> author)
  where
    EngineInfo {..} = engineInfo


parseErrorMsg :: Text
parseErrorMsg = "Error when parsing command"
