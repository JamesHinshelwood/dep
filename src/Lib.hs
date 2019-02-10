module Lib
    ( repl
    ) where

import Syntax
import Check
import Parser

import Control.Monad
import Data.Either

repl :: IO ()
repl = do
  input <- getContents
  let term = parseTerm input
  case typeOf [] term of
    Left err -> putStrLn $ show err
    Right ty -> do
      putStrLn $ show term
      putStrLn "has type"
      putStrLn $ show ty