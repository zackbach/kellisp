{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.Boolean where

import Kellisp.Environment.PrimUtils

booleanEnv :: [(Text, LispVal)]
booleanEnv = [ ("and", mkP $ foldM (boolBinop (&&)) $ Bool True)
             , ("or", mkP $ foldM (boolBinop (||)) $ Bool False)]

-- | Packages a boolean operator into LispVal form
boolBinop :: (Bool -> Bool -> Bool) -> BinOp
boolBinop op (Bool x) (Bool y) = return $ Bool $ op x y
boolBinop _ (Bool _) v = throw $ TypeMismatch "Expected boolean" v
boolBinop _ v _ = throw $ TypeMismatch " Expected boolean" v
