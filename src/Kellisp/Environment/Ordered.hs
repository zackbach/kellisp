{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Kellisp.Environment.Ordered (orderedEnv) where

import           Control.Exception

import qualified Data.Text as T

import           Kellisp.Environment.PrimUtils
import           Kellisp.Types

orderedEnv :: [(T.Text, LispVal)]
orderedEnv =
  [ ("=", mkP $ numOrd (==))
  , ("<", mkP $ numOrd (<))
  , ("<=", mkP $ numOrd (<=))
  , (">", mkP $ numOrd (>))
  , (">=", mkP $ numOrd (>=))
  , ("string=?", mkP $ strOrd (==))
  , ("string<?", mkP $ strOrd (<))
  , ("string<=?", mkP $ strOrd (<=))
  , ("string>?", mkP $ strOrd (>))
  , ("string>=?", mkP $ strOrd (>=))]

strOrd :: (forall a. Ord a => a -> a -> Bool) -> [LispVal] -> Eval LispVal
strOrd _ [] = throw $ NumArgs 1 []
strOrd _ [String _] = return $ Bool True
strOrd _ [v] = throw $ TypeMismatch "expected string" v
strOrd op vs = case strOrd' op vs of
  Left v  -> throw $ TypeMismatch "expected string" v
  Right b -> return $ Bool b
  where
    strOrd' :: (forall a. Ord a => a -> a -> Bool)
            -> [LispVal]
            -> Either LispVal Bool
    strOrd' op' (String x:String y:xs) = do
      rest <- strOrd' op' (String y:xs)
      return $ op' x y && rest
    -- we handle the empty case above
    strOrd' _ [String _] = Right True
    -- we catch all of the string cases above (empty, single, 2+),
    -- so this case means that we must have a type error
    strOrd' _ (String _:v:_) = Left v
    strOrd' _ (v:_) = Left v
    -- if this happens, something went wrong
    strOrd' _ _ = throw LispError

-- NOTE: it feels really bad to implement a dynamically typed language
-- from within a statically typed one, because I end up having to fight
-- against the type system like crazy...
-- | folds a numeric comparison operator across a non-empty list of LispVal
-- and accumulates the result, determining if it holds for every pair
numOrd :: (forall a. Ord a => a -> a -> Bool) -> [LispVal] -> Eval LispVal
numOrd _ [] = throw $ NumArgs 1 []
-- if there is only one numeric argument, return true
numOrd _ [Integer _] = return $ Bool True
numOrd _ [Double _] = return $ Bool True
numOrd _ [v] = throw $ TypeMismatch "expected numeric type" v
-- otherwise, evaluate and fold
numOrd op vs = case numOrd' op vs of
  Left v  -> throw $ TypeMismatch "expected numeric type" v
  Right b -> return $ Bool b
  where
    -- | Folds the comparison operator but does not wrap result, for recursive call
    -- if there is a type failure, returns the non-numeric value
    numOrd' :: (forall a. Ord a => a -> a -> Bool)
            -> [LispVal]
            -> Either LispVal Bool
    numOrd' op' (n:m:ns) = case numCast n m of
      -- this is super gross right now, the duplicated case
      -- seems hard to avoid given pattern match
      Right (Integer x, Integer y) -> do
        rest <- numOrd' op' (m:ns)
        return $ op' x y && rest
      Right (Double x, Double y) -> do
        rest <- numOrd' op' (m:ns)
        return $ op' x y && rest
      Left v -> Left v
      -- we aren't really supposed to throw here, but this also won't ever happen
      _ -> throw LispError
    -- since we checked empty case above, we can always return true here
    numOrd' _ _          = Right True
