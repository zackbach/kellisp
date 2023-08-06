{-|
Contains utilities useful for defining other primitive functions.
Re-exports relevant functions as well.
-}
module Kellisp.Environment.PrimUtils
    ( module Kellisp.Environment.PrimUtils
    , module Kellisp.Types
    , T.Text
    , throw
    , foldM) where

import           Control.Exception (throw)
import           Control.Monad (foldM)

import qualified Data.Text as T (Text)

import           Kellisp.Types

-- | Represents a unary operation over LispVal
type UnOp = LispVal -> Eval LispVal

-- | Represents a binary operation over LispVal
type BinOp = LispVal -> LispVal -> Eval LispVal

-- | Represents a trinary operation over LispVal
type TriOp = LispVal -> LispVal -> LispVal -> Eval LispVal

-- | Packages up a function into a PrimFun LispVal
mkP :: ([LispVal] -> Eval LispVal) -> LispVal
mkP = PrimFun . IFunc

-- | Runs a unary operator on a list of LispVals
mkUnop :: UnOp -> [LispVal] -> Eval LispVal
mkUnop op [v] = op v
mkUnop _ vs   = throw $ NumArgs 1 vs

-- | Runs a binary operator on a list of LispVals
-- we always take in a list of parameters, and this handles errors
mkBinop :: BinOp -> [LispVal] -> Eval LispVal
mkBinop op [x, y] = op x y
mkBinop _ vs      = throw $ NumArgs 2 vs

-- | runs a trinary operator on a list of LispVals
mkTriop :: TriOp -> [LispVal] -> Eval LispVal
mkTriop op [a, b, c] = op a b c
mkTriop _ vs         = throw $ NumArgs 3 vs

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
