-- |
-- Contains all outward-facing functions used to interact with Kellisp.
-- For now, this is limited to REPL functions.
module Kellisp where

import           Control.Monad.Reader
import qualified Data.Text as T
import           Kellisp.Env
import           Kellisp.Eval
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
evalVal :: Either String LispVal -> Either String (IO LispVal)
evalVal (Left err) = Left err
evalVal (Right val) = Right $ runReaderT (unEval (eval val)) env

-- | Pretty-prints some LispVal
printVal :: Either String (IO LispVal) -> IO ()
printVal (Left err) = putStrLn err
printVal (Right iolv) = iolv >>= print

-- | Reads Text as a LispVal, which is evaluated and printed
repVal :: T.Text -> IO ()
repVal = printVal . evalVal . readVal

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
          liftIO $ repVal $ T.pack input
          loop
