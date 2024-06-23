module Main where

import           AppPrelude

import           Models.Command
import           Parsers.Command     (parseCommand)
import           Uci                 (executeCommand, initialEngineState,
                                      putStrLnFlush)

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
      either (const $ putStrLnFlush parseErrorMsg) executeCommand
      . parseCommand


printHeader :: IO ()
printHeader =
  putStrLnFlush (name <> " " <> version <> " by " <> author)
  where
    EngineInfo {..} = engineInfo


parseErrorMsg :: Text
parseErrorMsg = "Error when parsing command"
