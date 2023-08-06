{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Kellisp.Types where

import           Control.Exception
import           Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Void (Void)

import           Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

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

-- | Represents an exception thrown by a LispVal process
data LispException =
    NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | NotFunction LispVal
  | UnboundVar T.Text
  | ParseError (ParseErrorBundle T.Text Void)
  | BadSpecialForm
  | LispError
  | DivByZero
  | EmptyList
    -- actual index -> left index (inclusive) -> right index (inclusive)
  | IndexOOB Integer Integer Integer
  deriving Eq

showException :: LispException -> T.Text
showException (NumArgs n args) = T.concat
  [ "Error: number of arguments\n"
  , "Expected "
  , T.pack $ show n
  , " arguments, but got "
  , T.pack $ show $ length args]
showException (TypeMismatch msg val) =
  T.concat ["Error: type mismatch\n", msg, ", but got ", showVal val]
showException (NotFunction val) = T.concat
  ["Error: expected function that can be applied, but got ", showVal val]
showException (UnboundVar var) =
  T.concat ["Error: ", var, " identifier is not bound"]
showException (ParseError bundle) =
  T.concat ["Error: parsing failure\n", T.pack $ errorBundlePretty bundle]
showException BadSpecialForm = "Error: bad special form not recognized"
showException LispError = "Error: the programmer of Kellisp made a mistake :("
showException DivByZero = "Error: cannot divide by zero"
showException EmptyList = "Error: expected a non-empty list, but got one"
showException (IndexOOB x l r) = T.concat
  [ "Error: index "
  , T.pack $ show x
  , " not in range ["
  , T.pack $ show l
  , ", "
  , T.pack $ show r
  , "] (inclusive)"]

instance Show LispException where
  show = T.unpack . showException

instance Exception LispException
