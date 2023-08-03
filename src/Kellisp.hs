{-# LANGUAGE ScopedTypeVariables #-}

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

-- | Reads and parses text into a LispVal that is evaluated
-- note that the Eval monad is not actually run here, so we
-- can pass in an initial context with `runReaderT` later
readEval :: T.Text -> Eval LispVal
readEval txt = case parse parseLispVal "" txt of
  Left bundle -> throw $ ParseError bundle
  Right val   -> eval val

-- | Read eval print loop
repl :: IO ()
repl = runInputT defaultSettings $ loop defaultEnv
  where
    loop :: Env -> InputT IO ()
    loop env = do
      s <- getInputLine "λ> "
      case s of
        -- if you wanted to add special commands, here is where that could be done
        -- just add another pattern for the Just case, like Just "quit", etc
        Nothing    -> return ()
        Just input -> do
          -- here, we parse the input with readEval and unwrap into a ReaderT monad to run
          -- but before we run, we also extract the environment to pass to the next loop
          -- and we wrap in a try to handle any errors that may arise
          res <- liftIO
            $ try
            $ runReaderT
              (liftM2 (,) ask (unEval (readEval $ T.pack input)))
              env
          case res of
            Left (err :: SomeException) -> outputStrLn (show err) >> loop env
            Right (env', val)           -> outputStrLn (show val) >> loop env'
