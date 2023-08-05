{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.NumericSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "arithmetic operations"
    $ do
      describe "addition"
        $ do
          it "can add two integers" $ "(+ 1 2)" `shouldEval` Integer 3
          it "can add two numbers" $ "(+ 1 2.0)" `shouldEval` Double 3.0
          it "can add one number" $ "(+ 1)" `shouldEval` Integer 1
          it "can add zero numbers" $ "(+)" `shouldEval` Integer 0
          it "can add many integers" $ "(+ 1 2 3)" `shouldEval` Integer 6
          it "can add many numbers" $ "(+ 1 2 3.5 4)" `shouldEval` Double 10.5
      describe "multiplication"
        $ do
          it "can multiply two integers" $ "(* 2 3)" `shouldEval` Integer 6
          it "can multiply two numbers" $ "(* 2 3.0)" `shouldEval` Double 6.0
          it "can multiply one number" $ "(* 2)" `shouldEval` Integer 2
          it "can multiply zero numbers" $ "(*)" `shouldEval` Integer 1
          it "can multiply many integers" $ "(* 2 3 4)" `shouldEval` Integer 24
          it "can multiply many numbers"
            $ "(* 1 2 3.0 4)" `shouldEval` Double 24.0

      describe "subtraction"
        $ do
          it "can subtract two integers" $ "(- 4 2)" `shouldEval` Integer 2
          it "can subtract two numbers"
            $ "(- 1 1.5)" `shouldEval` Double (-0.5)
          it "can negate a number" $ "(- 10)" `shouldEval` Integer (-10)
          it "can subtract many numbers"
            $ "(- 4 2 5)" `shouldEval` Integer (-3)
          it "doesn't subtract nothing"
            $ readRun "(-)" `shouldThrow` (== NumArgs 1 [])
      describe "division"
        $ do
          it "can divide two integers as a clean double"
            $ "(/ 4 2)" `shouldEval` Double 2.0
          it "can divide two integers as an uneven double"
            $ "(/ 3 2)" `shouldEval` Double 1.5
          it "can inverse one number" $ "(/ 4)" `shouldEval` Double 0.25
          it "doesn't divide nothing"
            $ readRun "(/)" `shouldThrow` (== NumArgs 1 [])
          it "doesn't divide by zero"
            $ readRun "(/ 4 2 0 1)" `shouldThrow` (== DivByZero)
