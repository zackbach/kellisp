{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.List (listEnv) where

import           Kellisp.Environment.PrimUtils

listEnv :: [(Text, LispVal)]
listEnv = [ ("cons", mkP $ mkBinop valCons)
          , ("car", mkP $ mkUnop valCar)
          , ("cdr", mkP $ mkUnop valCdr)]

-- | Cons operator for LispVal Lists
valCons :: BinOp
valCons v (List vs) = return $ List (v:vs)
valCons _ v         = throw $ TypeMismatch "Expected list" v

-- | `car` (head) operator for LispVal Lists
valCar :: UnOp
valCar (List (v:_)) = return v
valCar (List []) = throw EmptyList
valCar v = throw $ TypeMismatch "Expected list" v

-- These could have been abstracted but it's probably ok here
-- | `cdr` (tail) operator for LispVal lists
valCdr :: UnOp
valCdr (List (_:vs)) = return $ List vs
valCdr (List []) = throw EmptyList
valCdr v = throw $ TypeMismatch "Expected list" v
