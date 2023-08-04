{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Kellisp.Environment.Ordered (orderedEnv) where

import           Control.Monad (zipWithM)

import           Kellisp.Environment.Boolean (boolBinop)
import           Kellisp.Environment.PrimUtils

orderedEnv :: [(Text, LispVal)]
orderedEnv =
  [ ("=", mkP $ numOrdFold $ numOrdop (==))
  , ("<", mkP $ numOrdFold $ numOrdop (<))
  , ("<=", mkP $ numOrdFold $ numOrdop (<=))
  , (">", mkP $ numOrdFold $ numOrdop (>))
  , (">=", mkP $ numOrdFold $ numOrdop (>=))
  , ("string=?", mkP $ strOrdFold $ strOrdop (==))
  , ("string<?", mkP $ strOrdFold $ strOrdop (<))
  , ("string<=?", mkP $ strOrdFold $ strOrdop (<=))
  , ("string>?", mkP $ strOrdFold $ strOrdop (>))
  , ("string>=?", mkP $ strOrdFold $ strOrdop (>=))]

-- | Packages a comparison operator over numbers into LispVal form
numOrdop :: (forall a. Ord a => a -> a -> Bool) -> BinOp
numOrdop op n m = case numCast n m of
  Right (Integer x, Integer y) -> return $ Bool $ op x y
  Right (Double x, Double y) -> return $ Bool $ op x y
  Left v -> throw $ TypeMismatch "Expected numeric type" v
  _ -> throw LispError

-- | Folds a comparison BinOp across a (non-empty) list of numbers
-- determining whether the comparison holds for all successive numbers
numOrdFold :: BinOp -> [LispVal] -> Eval LispVal
numOrdFold _ []          = throw $ NumArgs 1 []
numOrdFold _ [Integer _] = return $ Bool True
numOrdFold _ [Double _]  = return $ Bool True
numOrdFold _ [v]         = throw $ TypeMismatch "Expected numeric type" v
-- we handle the empty and singleton list cases outside of the fold here
numOrdFold op vs         = andFold op vs

-- | Fold an operator across a list, comparing pairs then `and`ing together
andFold :: BinOp -> [LispVal] -> Eval LispVal
andFold op vs = do
   -- we basically make a list of pairs,
  -- then map the operator over them
  applied <- zipWithM op vs (tail vs)
  foldM (boolBinop (&&)) (Bool True) applied

-- | Packages a comparison operator over strings into LispVal form
strOrdop :: (forall a. Ord a => a -> a -> Bool) -> BinOp
strOrdop op (String x) (String y) = return $ Bool $ op x y
strOrdop _ (String _) v = throw $ TypeMismatch "Expected string" v
strOrdop _ v _ = throw $ TypeMismatch "Expected string" v

-- | Folds a comparison BinOp across a (non-empty) list of strings
-- determining whether the comparison holds for all successive strings
strOrdFold :: BinOp -> [LispVal] -> Eval LispVal
strOrdFold _ []         = throw $ NumArgs 1 []
strOrdFold _ [String _] = return $ Bool True
strOrdFold _ [v]        = throw $ TypeMismatch "Expected string" v
strOrdFold op vs        = andFold op vs
