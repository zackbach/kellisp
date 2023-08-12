{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.TypeConversion (conversionEnv) where

import qualified Data.Text as T

import Kellisp.Parser
import Kellisp.Environment.PrimUtils

import Text.Megaparsec (parse)

conversionEnv :: [(Text, LispVal)]
conversionEnv = [ ("number->string", mkP $ mkUnop numToString)
                , ("string->number", mkP $ mkUnop stringToNum)
                , ("symbol->string", mkP $ mkUnop symToString)
                , ("string->symbol", mkP $ mkUnop stringToSym)]

-- | converts a number LispVal to a String LispVal, if possible
numToString :: UnOp
numToString (Integer x) = return $ String $ T.pack $ show x
numToString (Double x)  = return $ String $ T.pack $ show x
numToString v = throw $ TypeMismatch "Expected a number" v

-- | converts a String LispVal to a Number LispVal, if possible
stringToNum :: UnOp
stringToNum (String s) = case parse parseLispVal "" s of
  Left bundle -> throw $ ParseError bundle
  Right (Integer x) -> return $ Integer x
  Right (Double x)  -> return $ Double x
  Right v -> throw $ TypeMismatch "Expected a number" v
stringToNum v = throw $ TypeMismatch "Expected a string" v

-- | converts an Atom LispVal to a String LispVal, if possible
symToString :: UnOp
symToString (Atom x) = return $ String x
symToString v = throw $ TypeMismatch "Expected a symbol" v

-- | returns a symbol with the name given by the provided String
-- as per the R5RS, this can create symbols with bad names
stringToSym :: UnOp
stringToSym (String s) = return $ Atom s
stringToSym v = throw $ TypeMismatch "Expected a string" v
