module Kellisp.Environment.PrimUtils (mkP, mkBinop) where

import           Control.Exception

import           Kellisp.Types

-- | Packages up a function into a PrimFun LispVal
mkP :: ([LispVal] -> Eval LispVal) -> LispVal
mkP = PrimFun . IFunc

-- | Runs a binary operator on a list of LispVals
-- we always take in a list of parameters, and this handles errors
mkBinop :: (LispVal -> LispVal -> Eval LispVal) -> [LispVal] -> Eval LispVal
mkBinop op [x, y] = op x y
mkBinop _ vs      = throw $ NumArgs 2 vs
