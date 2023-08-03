{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- | Here, we define a basic environment in which
-- our primitive functions are defined
module Kellisp.Environment.Numeric (numericEnv) where

import           Control.Exception
import           Control.Monad

import qualified Data.Text as T

import           Kellisp.Types
import           Kellisp.Environment.PrimUtils

numericEnv :: [(T.Text, LispVal)]
numericEnv = [ ("+", mkP $ foldM (numBinop (+)) (Integer 0))
             , ("*", mkP $ foldM (numBinop (*)) (Integer 1))
             , ("-", mkP $ mkBinop $ numBinop (-))
             , ("/", mkP $ mkBinop $ fracBinop (/))]

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

-- TODO: consider making this division-specific, allowing for div-by-0 error
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
