module Main where

import           AppPrelude

import           Models.Command
import           Parsers.Command     (parseCommand)
import           Uci                 (executeCommand)

import           Control.Monad.State
import qualified Data.Char           as Char


main :: IO ()
main = do
  printHeader
  st <- initialEngineState
  evalStateT (forever repl) st
  where
    repl = do
      input <- getLine
      unless (all Char.isSpace input)
              (eval input)
    eval =
      either (const $ putStrLn parseErrorMsg) executeCommand
      . parseCommand


printHeader :: IO ()
printHeader =
  putStrLn (name <> " " <> version <> " by " <> author)
  where
    EngineInfo {..} = engineInfo


parseErrorMsg :: Text
parseErrorMsg = "Error when parsing command"
