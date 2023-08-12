{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- | Here, we define a basic environment in which
-- our primitive functions are defined
module Kellisp.Environment.Numeric (numericEnv) where

import           Kellisp.Environment.PrimUtils

{-
TODO:
expt

number->string
string->number

STANDARD LIBRARY:
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
-- note: we only need Text due to the PrimUtils re-export
numericEnv :: [(Text, LispVal)]
numericEnv =
  [ ("+", mkP $ foldM (numBinop (+)) $ Integer 0)
  , ("*", mkP $ foldM (numBinop (*)) $ Integer 1)
  , ("-", mkP $ foldCase1 (numBinop (-)) $ numUnop negate)
  , ("/", mkP $ foldCase1 division $ doubleUnop recip)
  , ("quotient", mkP $ mkBinop $ intBinopDiv quot)
  , ("remainder", mkP $ mkBinop $ intBinopDiv rem)
  , ("modulo", mkP $ mkBinop $ intBinopDiv mod)
  , ("floor", mkP $ mkUnop $ doubleIntUnop floor)
  , ("ceiling", mkP $ mkUnop $ doubleIntUnop ceiling)
  , ("truncate", mkP $ mkUnop $ doubleIntUnop truncate)
  , ("round", mkP $ mkUnop $ doubleIntUnop round)
  , ("exp", mkP $ mkUnop $ doubleUnop exp)
  , ("sqrt", mkP $ mkUnop $ doubleUnop sqrt)
  -- for now, sqr always returns a double, but this will be reworked
  , ("sqr", mkP $ mkUnop $ doubleUnop (^^(2 :: Int)))
  , ("log", mkP $ mkUnop $ doubleUnop log)
  , ("sin", mkP $ mkUnop $ doubleUnop sin)
  , ("cos", mkP $ mkUnop $ doubleUnop cos)
  , ("tan", mkP $ mkUnop $ doubleUnop tan)
  , ("asin", mkP $ mkUnop $ doubleUnop asin)
  , ("acos", mkP $ mkUnop $ doubleUnop acos)
  , ("atan", mkP $ mkUnop $ doubleUnop atan)]

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

-- | Packages a unary numeric operation for doubles into LispVal form
doubleUnop :: (forall a. Floating a => a -> a) -> UnOp
doubleUnop op (Integer x) = return $ Double $ op $ fromInteger x
doubleUnop op (Double x) = return $ Double $ op x
doubleUnop _ v = throw $ TypeMismatch "Expected numeric type" v

-- | Packages a unary numeric operation for doubles into integers into LispVal
doubleIntUnop
  :: (forall a. RealFrac a => forall b. Integral b => a -> b) -> UnOp
doubleIntUnop op (Integer x) = return $ Integer $ op (fromInteger x :: Double)
doubleIntUnop op (Double x) = return $ Integer $ op x
doubleIntUnop _ v = throw $ TypeMismatch "Expected numeric type" v

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

-- | packages a binary integer operation into LispVal form
-- since this is specifically used for division operators,
-- an error is thrown if 0 is passed as the "denominator"
intBinopDiv :: (forall a. Integral a => a -> a -> a) -> BinOp
intBinopDiv _ _ (Integer 0) = throw DivByZero
intBinopDiv op (Integer x) (Integer y) = return $ Integer $ op x y
intBinopDiv _ (Integer _) v = throw $ TypeMismatch "Expected integer" v
intBinopDiv _ v _ = throw $ TypeMismatch "Expected integer" v
