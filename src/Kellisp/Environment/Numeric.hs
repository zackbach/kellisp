{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- | Here, we define a basic environment in which
-- our primitive functions are defined
module Kellisp.Environment.Numeric (numericEnv) where

import           Control.Exception
import           Control.Monad

import qualified Data.Text as T

import           Kellisp.Environment.PrimUtils
import           Kellisp.Types

{-
PREDICATES
number?
integer? (determine what to do with X.0)
double?

NUMBER NUMBER -> INTEGER
quotient
remainder
modulo

NUMBER -> INTEGER
floor
ceiling
truncate
round

MORE MATH
exp
expt
log
sin
cos
tan
asin
acos
atan
sqrt

number->string
string->number
-}
{-
library:
zero?
positive?
negative?
odd?
even?
max
min
abs
gcd
lcm
-}
numericEnv :: [(T.Text, LispVal)]
numericEnv = [ ("+", mkP $ foldM (numBinop (+)) $ Integer 0)
             , ("*", mkP $ foldM (numBinop (*)) $ Integer 1)
             , ("-", mkP $ foldCase1 (numBinop (-)) $ numUnop negate)
             , ("/", mkP $ foldCase1 division $ numUnop id)]

-- | Packages a binary numeric operation into LispVal form
numBinop :: (forall a. Num a => a -> a -> a) -> BinOp
numBinop op n m = case numCast n m of
  Right (Integer x, Integer y) -> return $ Integer $ op x y
  Right (Double x, Double y) -> return $ Double $ op x y
  Left v -> throw $ TypeMismatch "Expected numeric type" v
  -- this should never happen, but needed for exhaustive pattern match
  _ -> throw LispError

-- | Packages a unary numeric operation into LispVal form
numUnop :: (forall a. Num a => a -> a) -> UnOp
numUnop op (Integer x) = return $ Integer $ op x
numUnop op (Double x) = return $ Double $ op x
numUnop _ v = throw $ TypeMismatch "Expected numeric type" v

-- TODO: consider making this division-specific, allowing for div-by-0 error
-- | Packages an operation for Fractionals exclusively into LispVal form
division :: BinOp
division n m = case numCast n m of
  Right (_, Integer 0) -> throw DivByZero
  Right (_, Double 0) -> throw DivByZero
  Right
    (Integer x, Integer y) -> return $ Double $ fromInteger x / fromInteger y
  Right (Double x, Double y) -> return $ Double $ x / y
  Left v -> throw $ TypeMismatch "Expected numeric type" v
  -- this should never happen, but needed for exhaustive pattern match
  _ -> throw LispError
