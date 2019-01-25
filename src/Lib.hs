module Lib
    ( repl
    ) where

import Syntax
import Check
import Parser

import Control.Monad
import Text.Megaparsec
import Data.Either

repl :: IO ()
repl = do
  input <- getContents
  let prog = parseProg input
  case typeOf [] [] prog of
    Left err -> putStrLn $ show err
    Right ty -> putStrLn $ show ty
  repl