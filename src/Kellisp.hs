-- |
-- Contains all outward-facing functions used to interact with Kellisp.
-- For now, this is limited to REPL functions.
module Kellisp where

import qualified Data.Text as T
import           Kellisp.Parser
import           Kellisp.Types
import           System.Console.Haskeline
import           Text.Megaparsec (parse, errorBundlePretty)

-- | Reads Text into a LispVal
readVal :: T.Text -> Either String LispVal
readVal t = case parse parseLispVal "" t of
  Left bundle -> Left $ errorBundlePretty bundle
  Right val   -> Right val

-- TODO: consider how to handle errors more effectively here
-- | Evaluates a LispVal
eval :: Either String LispVal -> Either String LispVal
eval = id

-- | Pretty-prints some LispVal
printVal :: Either String LispVal -> String
printVal = either id show

-- | Reads Text as a LispVal, which is evaluated and "printed"
-- note, actually returned as a string and printing is postponed
rep :: T.Text -> String
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
        Nothing    -> return ()
        Just input -> do
          outputStrLn $ rep $ T.pack input
          loop
