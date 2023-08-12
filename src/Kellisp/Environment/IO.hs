{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.IO (ioEnv) where

import           Control.Monad.Reader

import qualified Data.ByteString as B (readFile)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

import           Kellisp.Environment.PrimUtils
import           Kellisp.Parser

import           Text.Megaparsec (parse)

ioEnv :: [(Text, LispVal)]
ioEnv = [ ("display", mkP $ mkUnop display)
        , ("slurp", mkP $ mkUnop slurp)
        , ("read", mkP $ mkUnop readVal)]

-- | Unary operator allowing for any LispVal to be displayed to stdout, returning Nil
display :: UnOp
display v = liftIO $ print v >> return Nil

-- | Unary operator allowing a file path to be read and returned
slurp :: UnOp
slurp (String path) = do
  bs <- liftIO $ B.readFile $ T.unpack path
  return $ String $ decodeUtf8 bs
slurp v = throw $ TypeMismatch "Expected string representing a filepath" v

-- | Reads in a string as a LispVal using the parser
readVal :: UnOp
readVal (String text) = do
  case parse parseLispVal "" text of
    Left bundle -> throw $ ParseError bundle
    Right val   -> return val
readVal v = throw $ TypeMismatch "Expected string" v
