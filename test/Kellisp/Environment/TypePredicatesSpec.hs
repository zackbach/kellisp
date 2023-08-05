{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.TypePredicatesSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "numeric type predicates"
    $ do
      it "determines integers are numbers"
        $ "(number? 1)" `shouldEval` Bool True
      it "determines doubles are numbers"
        $ "(number? 1.5)" `shouldEval` Bool True

      it "determines integers are integers"
        $ "(integer? 1)" `shouldEval` Bool True
      it "determines doubles aren't integers"
        $ "(integer? 1.5)" `shouldEval` Bool False

      it "determines integers aren't doubles"
        $ "(double? 1)" `shouldEval` Bool False
      it "determines doubles are doubles"
        $ "(double? 1.5)" `shouldEval` Bool True

  describe "false, nil, and empty predicates"
    $ do
      it "determines false is false" $ "(false? #f)" `shouldEval` Bool True
      it "determines nil isn't false" $ "(false? #nil)" `shouldEval` Bool False
      it "determines empty isn't false"
        $ "(false? '())" `shouldEval` Bool False

      it "determines false isn't nil" $ "(nil? #f)" `shouldEval` Bool False
      it "determines nil is nil" $ "(nil? #nil)" `shouldEval` Bool True
      it "determines empty isn't nil" $ "(nil? '())" `shouldEval` Bool False

      it "determines false isn't empty" $ "(empty? #f)" `shouldEval` Bool False
      it "determines nil isn't empty" $ "(empty? #nil)" `shouldEval` Bool False
      it "determines empty is empty" $ "(empty? '())" `shouldEval` Bool True

      it "determines nil isn't a boolean"
        $ "(boolean? #nil)" `shouldEval` Bool False
      it "determines false is a boolean"
        $ "(boolean? #f)" `shouldEval` Bool True
      it "detetmines empty isn't a boolean"
        $ "(boolean? '())" `shouldEval` Bool False

  describe "list predicates"
    $ do
      it "determines empty list is a list"
        $ "(list? '())" `shouldEval` Bool True
      it "determines non-empty list is a list"
        $ "(list? '(1 2 3))" `shouldEval` Bool True
      it "determines non-empty list isn't empty"
        $ "(empty? '(1 2 3))" `shouldEval` Bool False
      it "determines empty list isn't non-empty"
        $ "(cons? '())" `shouldEval` Bool False
      it "determines non-empty list is a non-empty list"
        $ "(cons? '(1 2 3))" `shouldEval` Bool True

  describe "symbols, predicates, and evaluation"
    $ do
      it "determines quotes symbols are symbols"
        $ "(symbol? '+)" `shouldEval` Bool True
      it "determines unquoted operators are procedures"
        $ "(procedure? +)" `shouldEval` Bool True
      it "evaluates operators before comparisons"
        $ "(procedure? (+ 1 2))" `shouldEval` Bool False
      it "determines lambdas are procedures" pending
      it "determines evaluated lambdas aren't procedures" pending

  describe "predicate general cases"
    $ do
      it "fails when too many arguments are applied"
        $ readRun "(string? \"a\" \"b\")"
        `shouldThrow` (== NumArgs 1 [String "a", String "b"])
      it "fails when too arguments are applied"
        $ readRun "(string?)" `shouldThrow` (== NumArgs 1 [])
      it "evaluates arguments before type predicate"
        $ "(boolean? (= 1 2))" `shouldEval` Bool True
