module Lib
    ( repl
    ) where

import Check
import Parser
import Print

import Unbound.LocallyNameless

repl :: IO ()
repl = do
  input <- getContents
  let term = parseTerm input
  let ty = runLFreshM $ typeOf [] term
  putStrLn $ show $ runLFreshM $ pp term
  putStrLn "has type"
  putStrLn $ show $ runLFreshM $ pp ty