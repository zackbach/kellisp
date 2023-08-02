{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Eval (eval) where

import           Control.Exception
import           Control.Monad.Reader

import qualified Data.Map as Map

import           Kellisp.Types

-- | Evaluates a LispVal using the Eval monad
-- note that we do not pass in the Env, since that is done
-- using runReaderT (after unEval to unwrap)
eval :: LispVal -> Eval LispVal
-- quote postpones evaluation for a single argument
-- TODO: figure out error handling for (quote 1 2 ...)
eval (List [Atom "quote", x]) = return x
-- some primitives are "autoquoted":
eval (Integer i) = return $ Integer i
eval (Double d) = return $ Double d
eval (Bool b) = return $ Bool b
eval Nil = return Nil
eval (List []) = return Nil
eval (Atom a) = do
  env <- ask -- ask the ReaderT for the stored env
  case Map.lookup a env of
    Just x  -> return x
    Nothing -> throw $ UnboundVar a
-- function application:
eval (List (f:args)) = do
  fun <- eval f -- evaluate the function
  args' <- mapM eval args -- evaluate all arguments
  case fun of
    -- pf :: IFunc :: [LispVal] -> Eval LispVal
    (PrimFun pf) -> fn pf args'
    -- for lambda, we evaluate in the stored context using local
    (Lambda lf ctx) -> local (const ctx) $ fn lf args'
    _ -> throw $ NotFunction fun
eval _ = throw BadSpecialForm
