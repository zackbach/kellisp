-- |
-- Contains all outward-facing functions used to interact with Kellisp.
-- For now, this is limited to REPL functions.
module Kellisp where

import System.Console.Haskeline

-- | For now, a LispVal is simply a String, but this will be expanded
type LispVal = String

-- | Reads Text into a LispVal
--  TODO: Update with Parser, may have to change signature to Text
readVal :: String -> LispVal
readVal = id

-- | Evaluates a LispVal
eval :: LispVal -> LispVal
eval = id

-- | Pretty-prints some LispVal
printVal :: LispVal -> String
printVal = id

-- | Reads Text as a LispVal, which is evaluated and printed
rep :: String -> String
rep = printVal . eval . readVal

-- | Read eval print loop
repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      s <- getInputLine "λ> "
      case s of
        -- if you wanted to add special commands, here is where that could be done
        -- just add another pattern for the Just case, like Just "quit", etc
        Nothing -> return ()
        Just input -> do
          outputStrLn $ rep input
          loop
