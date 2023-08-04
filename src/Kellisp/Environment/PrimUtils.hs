module Kellisp.Environment.PrimUtils where

import           Control.Exception
import           Control.Monad

import           Kellisp.Types

type UnOp = LispVal -> Eval LispVal

type BinOp = LispVal -> LispVal -> Eval LispVal

-- | Packages up a function into a PrimFun LispVal
mkP :: ([LispVal] -> Eval LispVal) -> LispVal
mkP = PrimFun . IFunc

-- | Runs a binary operator on a list of LispVals
-- we always take in a list of parameters, and this handles errors
mkBinop :: BinOp -> [LispVal] -> Eval LispVal
mkBinop op [x, y] = op x y
mkBinop _ vs      = throw $ NumArgs 2 vs

{-
-- | folds a binary operator through a list of LispVals,
-- associating to the left, with the leftmost value as the base
foldVal1 :: BinOp -> [LispVal] -> Eval LispVal
-- we have to make the type signature concrete so we can throw a LispException
foldVal1 _ [] = throw $ NumArgs 1 []
foldVal1 op (x:xs) = foldM op x xs
-}
-- | folds a binary operator through a non-empty list of LispVals,
-- but if there is only one value, applies the unary operator instead
foldCase1 :: BinOp -> UnOp -> [LispVal] -> Eval LispVal
foldCase1 _ _ []      = throw $ NumArgs 1 []
foldCase1 _ f [x]     = f x
foldCase1 op _ (x:xs) = foldM op x xs

-- | Casts two numbers into the more general type, if both numeric types
-- otherwise, returns the (first) value that could not be cast
numCast :: LispVal -> LispVal -> Either LispVal (LispVal, LispVal)
numCast (Integer x) val = case val of
  (Integer y) -> Right (Integer x, Integer y)
  (Double y)  -> Right (Double $ fromInteger x, Double y)
  y           -> Left y
numCast (Double x) val = case val of
  (Integer y) -> Right (Double x, Double $ fromInteger y)
  (Double y)  -> Right (Double x, Double y)
  y           -> Left y
numCast v _ = Left v
