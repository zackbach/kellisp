{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Eval (eval, evalBody) where

import           Control.Exception (throw)
import           Control.Monad.Reader

import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T

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

{-
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
-}
-- define as a shorthand for lambda
eval (List (Atom "define":rest)) = case rest of
  -- in this case, we bind the expr to the atom x
  [Atom x, expr] -> do
    expr' <- eval expr
    envRef <- ask
    liftIO $ modifyIORef envRef (Map.insert x expr')
    return expr'
  -- in this case, we have the lambda shorthand
  ((List ((Atom name):params)):body) -> eval
    -- we just rewrite as a define with a lambda
    (List [Atom "define", Atom name, List (Atom "lambda":List params:body)])
  [v, _] -> throw $ TypeMismatch "Expected an identifier" v
  _ -> throw $ BadSpecialForm "Expected variable(s) and expression or body"

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
      evalAndAdd _ (List [v, _]) =
        throw $ TypeMismatch "Expected an identifier" v
      evalAndAdd _ _ = throw $ BadSpecialForm "Expected a list of pairs"
  _ -> throw $ BadSpecialForm "Expected a list of pairs and a body expression"

eval (List (Atom "lambda":rest)) = case rest of
  [List _] -> throw $ BadSpecialForm "Expected a body expression"
  -- we can partially apply applyLambda to get an IFunc
  (List params:body) -> do
    idents <- mapM extractIdent params
    asks $ Lambda $ IFunc $ applyLambda body idents
  _ -> throw
    $ BadSpecialForm "Expected a list of parameters and a body expression"
  where
    -- | extracts an identifer name from an atom,
    -- throwing an error if the LispVal is not an Atom
    extractIdent :: LispVal -> Eval T.Text
    extractIdent (Atom a) = return a
    extractIdent v        = throw $ TypeMismatch "Expected an identifier" v

    -- | takes in a list of body expressions, a list of parameters,
    -- and a list of expressions to bind to parameters, then returns
    -- the body, evaluated with the arguments bound to parameters
    applyLambda :: [LispVal] -> [T.Text] -> [LispVal] -> Eval LispVal
    applyLambda body params args =
      if length params == length args
      then do
        -- we make a copy of the current environment so we don't bind globally
        curEnvRef <- ask
        curEnv <- liftIO $ readIORef curEnvRef
        scopedEnv <- liftIO $ newIORef curEnv
        -- bind the arguments to parameter identifiers
        zipWithM_
          (\idt val -> liftIO $ modifyIORef scopedEnv $ Map.insert idt val)
          params
          args
        -- then evaluate the body in the new scope
        local (const scopedEnv) $ evalBody body
      else throw $ NumArgs (toInteger $ length params) args

eval (List (f:args)) = do
  fun <- eval f -- evaluate the function
  args' <- mapM eval args -- evaluate all arguments
  case fun of
    -- pf :: IFunc :: [LispVal] -> Eval LispVal, extract with fn
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
