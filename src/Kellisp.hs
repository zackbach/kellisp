{-|
Contains all outward-facing functions used to interact with Kellisp.
For now, this is limited to REPL functions.
-}
module Kellisp where

-- |For now, a LispVal is simply a String, but this will be expanded
type LispVal = String

-- |Reads Text into a LispVal
-- TODO: Update with Parser, may have to change signature to Text
readVal :: String -> LispVal
readVal = id

-- |Evaluates a LispVal
eval :: LispVal -> LispVal
eval = id

-- |Pretty-prints some LispVal
printVal :: LispVal -> String
printVal = id

-- |Reads Text as a LispVal, which is evaluated and printed
rep :: String -> String
rep = printVal . eval . readVal

-- |Read eval print loop
repl :: IO ()
repl = do s <- getLine
          putStrLn $ rep s
          repl
