{-# LANGUAGE OverloadedStrings #-}

module Kellisp.EvalSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "auto-quoting of certain primitives"
    $ do
      it "evaluates #nil as nil" $ "#nil" `shouldEval` Nil
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

  describe "evaluation of if"
    $ do
      it "evaluates the second value when false"
        $ "(if #f 1 2)" `shouldEval` Integer 2
      it "doesn't evaluate the first value if false"
        $ "(if #f (/ 1 0) 2)" `shouldEval` Integer 2
      it "determines non-false values are truthy"
        $ "(if 0 1 2)" `shouldEval` Integer 1

  -- note: more thorough testing of function application comes with env
  describe "evaluation of function application"
    $ do
      it "applies functions to arguments" $ "(+ 1 2)" `shouldEval` Integer 3

  describe "evaluation of define"
    $ do
      it "assigns unbound variables"
        $ "(define x 4) x" `shouldEvalValues` Integer 4
      it "allows for updating variables"
        $ "(define x 1) (define x 2) x" `shouldEvalValues` Integer 2
      it "works with multiple variables"
        $ "(define x 1)(define y 2)(+ x y)" `shouldEvalValues` Integer 3

  describe "evaluation using begin"
    $ do
      it "evaluates sequential values" $ "(begin 1 2)" `shouldEval` Integer 2
      it "evaluates the same as if it were in a file"
        $ do
          b <- readRun "(begin (define x 1) (define y 2) (+ x y))"
          f <- readRunFile "(define x 1) (define y 2) (+ x y)"
          b `shouldBe` f
