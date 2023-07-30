module Kellisp.Types where

import qualified Data.Text as T (Text)

-- | Represents all values in the AST
data LispVal
  = Atom T.Text
  | List [LispVal]
  | Integer Integer
  | Double Double
  | String T.Text
  | Bool Bool
  | Nil
  -- later, we will add a pretty-printer, but this works for now
  deriving Show
