{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- | Here, we define a basic environment in which
-- our primitive functions are defined
module Kellisp.Env (defaultEnv) where

import           Control.Exception
import           Control.Monad

import qualified Data.Map as Map
import qualified Data.Text as T

import           Kellisp.Types

-- for now, we define primitives here, but they will likely be split up
-- we start by defining a large [(T.Text, LispVal)], then convert to Map
prims :: [(T.Text, LispVal)]
prims = [ ("+", mkP $ foldM (numBinop (+)) (Integer 0))
        , ("*", mkP $ foldM (numBinop (*)) (Integer 1))
        , ("-", mkP $ mkBinop $ numBinop (-))
        , ("/", mkP $ mkBinop $ fracBinop (/))]

-- | Represents the starting primitive environment that evaluation occurs in
defaultEnv :: Env
defaultEnv = Map.fromList prims

-- recall that Env is a synonym for Map.Map T.Text LispVal
-- we want to populate that map with PrimFun functions
-- | Packages up a function into a PrimFun LispVal
mkP :: ([LispVal] -> Eval LispVal) -> LispVal
mkP = PrimFun . IFunc

-- | Runs a binary operator on a list of LispVals
-- we always take in a list of parameters, and this handles errors
mkBinop :: (LispVal -> LispVal -> Eval LispVal) -> [LispVal] -> Eval LispVal
mkBinop op [x, y] = op x y
mkBinop _ vs      = throw $ NumArgs 2 vs

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

-- | Packages a numeric operation into LispVal form
numBinop
  :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> Eval LispVal
numBinop op n m = case numCast n m of
  Right (Integer x, Integer y) -> return $ Integer $ op x y
  Right (Double x, Double y) -> return $ Double $ op x y
  Left v -> throw $ TypeMismatch "Expected numeric type" v
  -- this should never happen, but needed for exhaustive pattern match
  _ -> throw LispError

-- | Packages an operation for Fractionals exclusively into LispVal form
fracBinop :: (forall a. Fractional a => a -> a -> a)
          -> LispVal
          -> LispVal
          -> Eval LispVal
fracBinop op n m = case numCast n m of
  Right (Integer x, Integer y)
    -> return $ Double $ op (fromInteger x) (fromInteger y)
  Right (Double x, Double y) -> return $ Double $ op x y
  Left v -> throw $ TypeMismatch "Expected numeric type" v
  -- this should never happen, but needed for exhaustive pattern match
  _ -> throw LispError
