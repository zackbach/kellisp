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

  describe "division operations"
    $ do
      it "quotients numbers evenly" $ "(quotient 4 2)" `shouldEval` Integer 2
      it "rounds down for quotient" $ "(quotient 4 3)" `shouldEval` Integer 1
      it "doesn't allow zero division"
        $ readRun "(quotient 4 0)" `shouldThrow` (== DivByZero)
      it "doesn't quotient doubles"
        -- note: (quotient 4 2.0) could be cast, but it currently errors as well
        $ readRun "(quotient 4 1.5)"
        `shouldThrow` (== TypeMismatch "Expected integer" (Double 1.5))

      -- the tests above test division by zero and type issues, so we don't redo
      it "modulo takes sign of second"
        $ "(modulo -13 4)" `shouldEval` Integer 3
      it "remainder takes sign of first"
        $ "(remainder -13 4)" `shouldEval` Integer (-1)

  -- NOTE: tests are missing for exp, log, sqrt, and trig functions
  -- since they return doubles and equality is annoying
  -- also, they are just taken directly from Haskell standard library

  describe "rounding operations"
    $ do
      it "truncate rounds down for positive numbers"
        $ "(truncate 3.5)" `shouldEval` Integer 3
      it "truncate rounds up for negative numbers"
        $ "(truncate -4.3)" `shouldEval` Integer (-4)

      it "floor rounds down for positive numbers"
        $ "(floor 3.5)" `shouldEval` Integer 3
      it "floor rounds down for negative numbers"
        $ "(floor -4.3)" `shouldEval` Integer (-5)

      it "ceiling rounds up for positive numbers"
        $ "(ceiling 3.5)" `shouldEval` Integer 4
      it "ceiling rounds up for negative numbers"
        $ "(ceiling -4.3)" `shouldEval` Integer (-4)

      it "round rounds to the closer integer"
        $ "(round -4.3)" `shouldEval` Integer (-4)
      it "round rounds down to even number for .5"
        $ "(round 3.5)" `shouldEval` Integer 4
      it "round rounds up to even number for .5"
        $ "(round 4.5)" `shouldEval` Integer 4
