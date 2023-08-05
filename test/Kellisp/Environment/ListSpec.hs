{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.ListSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "cons"
    $ do
      it "adds values on to empty list"
        $ "(cons 1 '())" `shouldEval` List [Integer 1]
      it "adds values onto non-empty list"
        $ "(cons 2 '(1))" `shouldEval` List [Integer 2, Integer 1]
      it "adds values regardless of type"
        $ "(cons \"a\" '(1))" `shouldEval` List [String "a", Integer 1]
      it "adds lists on to lists"
        $ "(cons '(a) '(b c))"
        `shouldEval` List [List [String "a"], String "b", String "c"]

  describe "car"
    $ do
      it "takes the head of a non-empty list"
        $ "(car '(1 2 3))" `shouldEval` Integer 1
      it "takes the head of a nested list"
        $ "(car '((a b) c d))" `shouldEval` List [Atom "a", Atom "b"]
      it "doesn't allow non-empty lists"
        $ readRun "(car '())" `shouldThrow` (== EmptyList)

  describe "cdr"
    $ do
      it "takes the tail of a non-empty list"
        $ "(cdr '(1 2 3))" `shouldEval` List [Integer 2, Integer 3]
      it "takes the tail of a nested list"
        $ "(cdr '((a b) c d))" `shouldEval` List [Atom "c", Atom "d"]
      it "takes the tail of a singleton list"
        $ "(cdr '(a))" `shouldEval` List []
      it "doesn't allow non-empty lists"
        $ readRun "(cdr '())" `shouldThrow` (== EmptyList)
