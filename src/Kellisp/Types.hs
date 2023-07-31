{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Kellisp.Types where

import           Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as Map

-- | Represents all values in the AST
data LispVal =
    Atom T.Text
  | List [LispVal]
  | Integer Integer
  | Double Double
  | String T.Text
  | Bool Bool
  | Nil
    -- represents a primitive function
  | PrimFun IFunc
    -- represents a defined function that
    -- stores its environment for lexical scoping
  | Lambda IFunc Env
  deriving Eq

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
showVal (PrimFun _)  = "<primitive>"
showVal (Lambda _ _) = "<lambda>"

instance Show LispVal where
  show = T.unpack . showVal


-- | Represents an environment associating Text identifiers with bound LispVals
type Env = Map.Map T.Text LispVal

-- | Represents a standard function
-- taking in a function from a list of parameters to the result
newtype IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

-- to make deriving Eq work above, we have to define equality over functions,
-- which is not possible, so two functions are always not equal
instance Eq IFunc where
  _ == _ = False

-- | Represents an evaluation monad,
-- where a ReaderT monad transformer wraps an underlying IO monad
-- and the read-only environment is the Env above (allows lexical scoping)
newtype Eval a = Eval { unEval :: ReaderT Env IO a }
-- by using record syntax, we get an accessor to easily unwrap
-- we also use GeneralizedNewtypeDeriving to derive these easily
-- by doing so, we don't have to lift as much
  deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO)

