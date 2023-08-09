{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Contains all outward-facing functions used to interact with Kellisp.
-- For now, this is limited to REPL functions.
module Kellisp where

import           Control.Exception
import           Control.Monad.Reader

import qualified Data.ByteString as B (readFile)
import           Data.IORef (newIORef)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

import           Kellisp.Environment (defaultEnv)
import           Kellisp.Eval
import           Kellisp.Parser
import           Kellisp.Types

import           System.Console.Haskeline

import           Text.Megaparsec (parse)

-- | Read eval print loop in the default environment
repl :: IO ()
repl = do
  defaultEnvRef <- newIORef defaultEnv
  runInputT defaultSettings $ loop defaultEnvRef

-- | Reads the given file, if it exists, and loads definitions into a repl
fileRepl :: FilePath -> IO ()
fileRepl fp = do
  envref <- newIORef defaultEnv
  _ <- evalInputFile envref fp
  runInputT defaultSettings $ loop envref

-- | one iteration of the REPL, evaluated in the given environment
-- mutually recursive with handleInput
loop :: EnvRef -> InputT IO ()
loop env = do
  s <- getInputLine "λ> "
  case s of
    -- if you wanted to add special commands, here is where that could be done
    -- just add another pattern for the Just case, like Just "quit", etc
    Nothing    -> return ()
    Just input -> handleInput env $ T.pack input

-- | attempts to evaluate the input in the given environment, catching errors
-- and passing in the updated environment to the next loop iteration
handleInput :: EnvRef -> T.Text -> InputT IO ()
handleInput env input = do
  res <- liftIO $ try $ evalInput env $ readEval input
  -- we specify the type of the first show to prevent ambigious type variable
  -- error arising from the try above
  outputStrLn (either (show :: SomeException -> String) show res)
  loop env

-- | parses and evaluates text in the given input, returning
-- both the resulting LispVal and the new environment
evalInput :: EnvRef -> Eval LispVal -> IO LispVal
evalInput env v = runReaderT (unEval v) env

-- | Reads the given file, if it exists, and evaluates it in the given
-- environment, updating that environment
evalInputFile :: EnvRef -> FilePath -> IO LispVal
evalInputFile envref fp = do
  bs <- B.readFile fp
  -- turns the file bytestring into text and evaluates, updating envref
  evalInput envref $ readEvalFile $ decodeUtf8 bs

-- | Reads and parses text into a LispVal that is evaluated
-- note that the Eval monad is not actually run here, so we
-- can pass in an initial context with `runReaderT` later
readEval :: T.Text -> Eval LispVal
readEval txt = case parse parseLispVal "" txt of
  Left bundle -> throw $ ParseError bundle
  Right val   -> eval val

-- | Reades and parses text from a file into a LispVal that is evaluated
readEvalFile :: T.Text -> Eval LispVal
readEvalFile txt = case parse parseLispValues "" txt of
  Left bundle  -> throw $ ParseError bundle
  Right values -> evalBody values
