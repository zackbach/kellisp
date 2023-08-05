{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.TypePredicates (predicateEnv) where

import           Kellisp.Environment.PrimUtils

predicateEnv :: [(Text, LispVal)]
predicateEnv =
  [ ("number?", mkP $ mkUnop numberPred)
  , ("integer?", mkP $ mkUnop integerPred)
  , ("double?", mkP $ mkUnop doublePred)
  , ("boolean?", mkP $ mkUnop boolPred)
  , ("false?", mkP $ mkUnop falsePred)
  , ("symbol?", mkP $ mkUnop atomPred)
  , ("list?", mkP $ mkUnop listPred)
  , ("empty?", mkP $ mkUnop emptyPred)
  , ("cons?", mkP $ mkUnop consPred)
  , ("string?", mkP $ mkUnop stringPred)
  , ("procedure?", mkP $ mkUnop procPred)
  , ("nil?", mkP $ mkUnop nilPred)]

-- | determines whether a given LispVal is a number
numberPred :: UnOp
numberPred (Integer _) = return $ Bool True
numberPred (Double _)  = return $ Bool True
numberPred _           = return $ Bool False

-- | determines whether a given LispVal is an integer
-- currently, (integer? 1.0) is false, but this could be changed
integerPred :: UnOp
integerPred (Integer _) = return $ Bool True
integerPred _           = return $ Bool False

-- | determines whether a given LispVal is a double
doublePred :: UnOp
doublePred (Double _) = return $ Bool True
doublePred _          = return $ Bool False

-- | determines whether a given LispVal is a boolean
boolPred :: UnOp
boolPred (Bool _) = return $ Bool True
boolPred _        = return $ Bool False

-- | determines whether a given LispVal is false
falsePred :: UnOp
falsePred (Bool False) = return $ Bool True
falsePred _ = return $ Bool False

-- | determines whether a given LispVal is a symbol / atom
atomPred :: UnOp
atomPred (Atom _) = return $ Bool True
atomPred _        = return $ Bool False

-- | determines whether a given LispVal is a list
listPred :: UnOp
listPred (List _) = return $ Bool True
listPred _        = return $ Bool False

-- | determines whether a given LispVal is an empty list
emptyPred :: UnOp
emptyPred (List []) = return $ Bool True
emptyPred _         = return $ Bool False

-- | determines whether a given LispVal is a non-empty list
consPred :: UnOp
consPred (List (_:_)) = return $ Bool True
consPred _ = return $ Bool False

-- | determines whether a given LispVal is a string
stringPred :: UnOp
stringPred (String _) = return $ Bool True
stringPred _          = return $ Bool False

-- | determines whether a given LispVal is a procedure
procPred :: UnOp
procPred (PrimFun _) = return $ Bool True
procPred (Lambda _ _) = return $ Bool True
procPred _ = return $ Bool False

-- | determines whether a given LispVal is nil
nilPred :: UnOp
nilPred Nil = return $ Bool True
nilPred _   = return $ Bool False

