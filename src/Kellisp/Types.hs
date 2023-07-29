module Kellisp.Types where

import Data.Text qualified as T (Text)

-- | Represents all values in the AST
data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Bool Bool
  | Nil

{- Later, can include
DottedList [LispVal] LispVal
Characters
Floats / more numbers
Vectors
Hash-maps
-}

-- figure out how to handle comments too: what should be returned?
