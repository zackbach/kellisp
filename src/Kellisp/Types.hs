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
  -- later, we will add a pretty-printer, but this works for now
  deriving Show
