{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Types where

import qualified Data.Text as T

-- | Represents all values in the AST
data LispVal = Atom T.Text
             | List [LispVal]
             | Integer Integer
             | Double Double
             | String T.Text
             | Bool Bool
             | Nil

-- | Pretty-prints a LispVal as Text
showVal :: LispVal -> T.Text
showVal (Atom a)     = a
showVal (List l)     = T.concat ["(", T.unwords (map showVal l), ")"]
showVal (Integer i)  = T.pack $ show i
showVal (Double d)   = T.pack $ show d
showVal (String s)   = T.concat ["\"", s, "\""]
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal Nil          = "nil"

instance Show LispVal where
  show = T.unpack . showVal
