{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.String where

import qualified Data.Text as T

import           Kellisp.Environment.PrimUtils

stringEnv :: [(T.Text, LispVal)]
stringEnv = [ ("string-append", mkP $ foldM (textBinop T.append) $ String "")
            , ("string-length", mkP $ mkUnop textLength)
            , ("string-ref", mkP $ mkBinop textIndex)
            , ("substring", mkP $ mkTriop textSubstring)]

-- | Packages a binary text operation into LispVal form
textBinop :: (Text -> Text -> Text) -> BinOp
textBinop op (String a) (String b) = return $ String $ op a b
textBinop _ (String _) v = throw $ TypeMismatch "Expected string" v
textBinop _ v _ = throw $ TypeMismatch "Expected string" v

-- since all signatures are different here, I am just hard-coding them
-- | `length` operator for LispVal Strings
textLength :: UnOp
textLength (String s) = return $ Integer $ toInteger $ T.length s
textLength v          = throw $ TypeMismatch "Expected string" v

-- | `elementAt` operator for LispVal Strings, taking in a string and integer
textIndex :: BinOp
textIndex (String s) (Integer n) =
  if 0 <= n && n < toInteger (T.length s)
  then return $ String $ T.singleton $ T.index s $ fromInteger n
  else throw $ IndexOOB n 0 $ toInteger (T.length s) - 1
textIndex (String _) v = throw $ TypeMismatch "Expected integer" v
textIndex v _ = throw $ TypeMismatch "Expected string" v

-- | `substring` operator for LispVal Strings, taking in a string,
-- a(n inclusive) starting index, and an (exclusive) ending index as integers
textSubstring :: TriOp
textSubstring (String s) (Integer l) (Integer r) =
  -- we compare the indices separately for better errors
  if 0 <= l && l <= n
  then if l <= r && r <= n
       then
         -- since there is no good built-in substring, you have to do drop / take
         return
         $ String
         -- here, we take to get rid of everything to the right of r
         $ T.take (fromInteger $ r - l)
         -- here, we drop to get rid of everything to the left of l
         $ T.drop (fromInteger l) s
       else throw $ IndexOOB r l n
  else throw $ IndexOOB l 0 n
  where
    n = toInteger $ T.length s
textSubstring (String _) (Integer _) v =
  throw $ TypeMismatch "Expected integer" v
textSubstring (String _) v _ = throw $ TypeMismatch "Expected integer" v
textSubstring v _ _ = throw $ TypeMismatch "Expected string" v
