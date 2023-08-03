-- |
-- Contains all outward-facing functions used to interact with Kellisp.
-- For now, this is limited to REPL functions.
module Kellisp where

import           Control.Exception
import           Control.Monad.Reader

import qualified Data.Text as T

import           Kellisp.Env
import           Kellisp.Eval
import           Kellisp.Parser
import           Kellisp.Types

import           System.Console.Haskeline

import           Text.Megaparsec (parse)

-- | Read eval print loop
repl :: IO ()
repl = runInputT defaultSettings $ loop defaultEnv

-- | one iteration of the REPL, evaluated in the given environment
-- mutually recursive with handleInput
loop :: Env -> InputT IO ()
loop env = do
  s <- getInputLine "λ> "
  case s of
    -- if you wanted to add special commands, here is where that could be done
    -- just add another pattern for the Just case, like Just "quit", etc
    Nothing    -> return ()
    Just input -> handleInput env $ T.pack input

-- TODO: these helpers could probably be cleaned up a bit
-- and their signatures could be enhanced / generalized

-- | attempts to evaluate the input in the given environment, catching errors
-- and passing in the updated environment to the next loop iteration
handleInput :: Env -> T.Text -> InputT IO ()
handleInput env input = do
  res <- liftIO $ try $ evalInput env input
  case res of
    Left (err :: SomeException) -> outputStrLn (show err) >> loop env
    Right (env', val)           -> outputStrLn (show val) >> loop env'

-- | parses and evaluates text in the given input, returning
-- both the resulting LispVal and the new environment
evalInput :: Env -> T.Text -> IO (Env, LispVal)
evalInput env input = runReaderT comp env
  where
    comp = do
      result <- unEval $ readEval input
      evalEnv <- ask
      return (evalEnv, result)

-- | Reads and parses text into a LispVal that is evaluated
-- note that the Eval monad is not actually run here, so we
-- can pass in an initial context with `runReaderT` later
readEval :: T.Text -> Eval LispVal
readEval txt = case parse parseLispVal "" txt of
  Left bundle -> throw $ ParseError bundle
  Right val   -> eval val

