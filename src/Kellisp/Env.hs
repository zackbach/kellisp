{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- | Here, we define a basic environment in which
-- our primitive functions are defined
module Kellisp.Env (env) where

import qualified Data.Map as Map
import qualified Data.Text as T
import           Kellisp.Types

-- for now, we define primitives here, but they will likely be split up
-- we start by defining a large [(T.Text, LispVal)], then convert to Map
prims :: [(T.Text, LispVal)]
prims = [ ("+", mkP $ mkBinop $ numBinop (+))
        , ("*", mkP $ mkBinop $ numBinop (*))
        , ("-", mkP $ mkBinop $ numBinop (-))
        , ("/", mkP $ mkBinop $ fracBinop (/))]

-- | Represents the starting primitive environment that evaluation occurs in
env :: Env
env = Map.fromList prims

-- recall that Env is a synonym for Map.Map T.Text LispVal
-- we want to populate that map with PrimFun functions
-- | Packages up a function into a PrimFun LispVal
mkP :: ([LispVal] -> Eval LispVal) -> LispVal
mkP = PrimFun . IFunc

-- | Runs a binary operator on a list of LispVals
mkBinop :: (LispVal -> LispVal -> Eval LispVal) -> [LispVal] -> Eval LispVal
mkBinop op [x, y] = op x y
-- for now, return Nil to handle errors
mkBinop _ _       = return Nil

-- | Packages a numeric operation into LispVal form
numBinop
  :: (forall a. Num a => a -> a -> a) -> LispVal -> LispVal -> Eval LispVal
numBinop op n m = case numCast n m of
  Just (Integer x, Integer y) -> return $ Integer $ op x y
  Just (Double x, Double y) -> return $ Double $ op x y
  -- TODO: figure out better error handling
  _ -> return Nil

-- | Casts two numbers into the more general type
-- NOTE: this may have to return a LispVal (package into a LispVal List)
-- in order to support actual error handling
numCast :: LispVal -> LispVal -> Maybe (LispVal, LispVal)
numCast (Integer x) (Integer y) = Just (Integer x, Integer y)
numCast (Double x) (Double y) = Just (Double x, Double y)
numCast (Double x) (Integer y) = Just (Double x, Double (fromInteger y))
numCast (Integer x) (Double y) = Just (Double (fromInteger x), Double y)
numCast _ _ = Nothing

-- | Packages an operation for Fractionals exclusively into LispVal form
fracBinop :: (forall a. Fractional a => a -> a -> a)
          -> LispVal
          -> LispVal
          -> Eval LispVal
fracBinop op n m = case numCast n m of
  Just (Integer x, Integer y)
    -> return $ Double $ op (fromInteger x) (fromInteger y)
  Just (Double x, Double y) -> return $ Double $ op x y
  _ -> return Nil
