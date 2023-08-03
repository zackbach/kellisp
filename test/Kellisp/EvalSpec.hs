{-# LANGUAGE OverloadedStrings #-}

module Kellisp.EvalSpec where

import           Kellisp.Types
import           Kellisp.SpecUtils

import           Test.Hspec

spec :: Spec
spec = do
  describe "auto-quoting of certain primitives"
    $ do
      it "evaluates nil as nil" $ "nil" `shouldEval` Nil
      it "evaluates #t as #t" $ "#t" `shouldEval` Bool True
      it "evaluates #f as #f" $ "#f" `shouldEval` Bool False
      it "evaluates integers as integers" $ "123" `shouldEval` Integer 123
      it "evaluates doubles as doubles" $ "123.45" `shouldEval` Double 123.45
      it "evaluates strings as strings"
        $ "\"hi mom\"" `shouldEval` String "hi mom"

  describe "evaluation of quote"
    $ do
      it "evaluates quoted atoms" $ "'a" `shouldEval` Atom "a"
      it "evaluates quoted lists"
        $ "'(1 2)" `shouldEval` List [Integer 1, Integer 2]
      it "evaluates quoted items"
        $ "(quote 1 2)" `shouldEval` List [Integer 1, Integer 2]

  -- note: more thorough testing of function application comes with env
  describe "evaluation of function application"
    $ do
      it "applies functions to arguments" $ "(+ 1 2)" `shouldEval` Integer 3
