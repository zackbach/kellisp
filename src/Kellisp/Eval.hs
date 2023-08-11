{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Eval (eval, evalBody) where

import           Control.Exception (throw)
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

eval (List (Atom "begin":vs)) = evalBody vs

eval (List (Atom "if":vs)) = case vs of
  [condition, ifTrue, ifFalse] -> do
    c <- eval condition
    case c of
      -- all values are truthy except #f
      (Bool False) -> eval ifFalse
      _ -> eval ifTrue
  _ -> throw $ BadSpecialForm "Expected a condition and two expressions"

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
    v        -> throw $ TypeMismatch "Expected symbol" v

-- TODO: try to abstract similarities between the let cases
eval (List (Atom "let":vs)) = case vs of
  -- we check for the singleton list case first, because body could be []
  [List _] -> throw $ BadSpecialForm "Expected a body expression"
  ((List pairs):body) -> do
    -- make a copy of the current environment
    curEnvRef <- ask
    curEnv <- liftIO $ readIORef curEnvRef
    scopedEnv <- liftIO $ newIORef curEnv
    -- evaluate the initial values assigned to variables then assign in env
    mapM_ (evalThenAdd scopedEnv) pairs
    -- evaluate the body in the new environment
    local (const scopedEnv) $ evalBody body
    where
      -- | evaluates the LispVal in the current environment then
      -- adds it to that environment with the extracted identifier
      evalThenAdd :: EnvRef -> LispVal -> Eval ()
      evalThenAdd envref (List [Atom t, valExpr]) = do
        val <- eval valExpr
        liftIO $ modifyIORef envref (Map.insert t val)
      evalThenAdd _ _ = throw $ BadSpecialForm "Expected a list of pairs"

  _ -> throw $ BadSpecialForm "Expected a list of pairs and a body expression"

eval (List (Atom "let*":vs)) = case vs of
  [List _] -> throw $ BadSpecialForm "Expected a body expression"
  ((List pairs):body) -> do
    -- make a copy of the current environment (this could be made into helper)
    curEnvRef <- ask
    curEnv <- liftIO $ readIORef curEnvRef
    scopedEnv <- liftIO $ newIORef curEnv
    -- we use evalAndAdd, adding the values to the environment as we go for let*
    mapM_ (evalAndAdd scopedEnv) pairs
    -- finally, evaluate the body in the new environment
    local (const scopedEnv) $ evalBody body
    where
          -- this is where let* is different from let
      -- | evaluates the LispVal in the given environment then
      -- adds it to that environment with the extracted identifier
      evalAndAdd :: EnvRef -> LispVal -> Eval ()
      evalAndAdd envref (List [Atom t, valExpr]) = do
        -- unlike let, we evaluate in the new environment for let*
        val <- local (const envref) $ eval valExpr
        liftIO $ modifyIORef envref (Map.insert t val)
      evalAndAdd _ _ = throw $ BadSpecialForm "Expected a list of pairs"
  _ -> throw $ BadSpecialForm "Expected a list of pairs and a body expression"

eval (List (f:args)) = do
  fun <- eval f -- evaluate the function
  args' <- mapM eval args -- evaluate all arguments
  case fun of
    -- pf :: IFunc :: [LispVal] -> Eval LispVal
    (PrimFun pf) -> fn pf args'
    -- for lambda, we evaluate in the stored context using local
    (Lambda lf ctx) -> local (const ctx) $ fn lf args'
    _ -> throw $ NotFunction fun

eval _ = throw $ BadSpecialForm "Form is not recognized / implemented"

-- | Evaluates a body expression by evaluating all LispValues
-- sequentially, then returning the final result
evalBody :: [LispVal] -> Eval LispVal
-- not sure what to do with the empty case... maybe make better error
evalBody []     = throw $ NumArgs 1 []
evalBody [v]    = eval v
evalBody (v:vs) = eval v >> evalBody vs
