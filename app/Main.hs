module Main where

import           Kellisp (repl, fileRepl)

import           System.Environment

-- |Interpreter entry point: enters the REPL
main :: IO ()
main = do
  args <- getArgs
  case args of
    []   -> repl
    [fp] -> fileRepl fp
    _    -> fail "Error: input either zero or one arguments"
