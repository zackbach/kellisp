module Main where

import           Kellisp (repl)

-- |Interpreter entry point: enters the REPL
-- TODO: parse command line arguments, either REPL or interpret a file
main :: IO ()
main = repl
