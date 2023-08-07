{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Eval (eval) where

import           Control.Exception
import           Control.Monad.Reader

import           Data.IORef
import qualified Data.Map as Map

import           Kellisp.Types

-- | Evaluates a LispVal using the Eval monad
-- note that we do not pass in the Env, since that is done
-- using runReaderT (after unEval to unwrap)
eval :: LispVal -> Eval LispVal
-- quote postpones evaluation for arguments
eval (List [Atom "quote", x]) = return x
eval (List (Atom "quote":xs)) = return $ List xs
-- some primitives are "autoquoted":
eval (Integer i) = return $ Integer i
eval (Double d) = return $ Double d
eval (Bool b) = return $ Bool b
eval (String s) = return $ String s
eval Nil = return Nil
eval (List []) = return Nil

eval (Atom a) = do
  envref <- ask
  env <- liftIO $ readIORef envref
  case Map.lookup a env of
    Just x  -> return x
    Nothing -> throw $ UnboundVar a
eval (List (Atom "if":vs)) = do
  case vs of
    [condition, ifTrue, ifFalse] -> do
      c <- eval condition
      case c of
        -- all values are truthy except #f
        (Bool False) -> eval ifFalse
        _  -> eval ifTrue
    _ -> throw BadSpecialForm
-- TODO: prevent define from inside of expressions?
-- for now we just return what the identifier was bound to
eval (List [Atom "define", var, expr]) = do
  case var of
    (Atom x) -> do
      expr' <- eval expr
      envRef <- ask
      -- here, we modify the stored reference to the environment
      -- by adding in the newly defined identifier x
      liftIO $ modifyIORef envRef (Map.insert x expr')
      return expr'
    v -> throw $ TypeMismatch "Expected symbol" v

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
