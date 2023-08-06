{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.OrderedSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "numeric comparisons"
    $ do
      -- note that folding behavior is only tested for equality,
      -- since all others use the same behavior
      describe "equality"
        $ do
          it "compares two integers for equality"
            $ "(= 1 1)" `shouldEval` Bool True
          it "compares two numbers for equality"
            $ "(= 1 1.0)" `shouldEval` Bool True
          it "takes at least one argument"
            $ readRun "(=)" `shouldThrow` (== NumArgs 1 [])
          it "takes one argument" $ "(= 1)" `shouldEval` Bool True
          it "takes many arguments" $ "(= 1 1 1 1 1)" `shouldEval` Bool True
          it "carries over failures"
            $ "(= 1 1 1 2 1 1 1)" `shouldEval` Bool False
          it "properly finds type errors"
            $ readRun "(= 1 1 \"a\" 1 \"b\" 1)"
            `shouldThrow` (== TypeMismatch "Expected numeric type" (String "a"))

      describe "comparisons"
        $ do
          it "compares two integers with <" $ "(< 1 1)" `shouldEval` Bool False
          it "compares two integers with <="
            $ "(<= 1 1)" `shouldEval` Bool True
          it "compares two integers with >" $ "(> 1 1)" `shouldEval` Bool False
          it "compares two integers with >="
            $ "(>= 1 1)" `shouldEval` Bool True
          it "compares two numbers with <" $ "(< 1 2.0)" `shouldEval` Bool True
          it "compares two numbers with >"
            $ "(> 4 -3.0)" `shouldEval` Bool True

  describe "string comparisons"
    $ do
      describe "equality"
        $ do
          it "compares two strings for equality"
            $ "(string=? \"a\" \"a\")" `shouldEval` Bool True
          it "takes at least one argument"
            $ readRun "(string=?)" `shouldThrow` (== NumArgs 1 [])
          it "takes one argument" $ "(string=? \"a\")" `shouldEval` Bool True
          it "takes many arguments"
            $ "(string=? \"a\" \"a\" \"a\" \"a\")" `shouldEval` Bool True
          it "carries over failures"
            $ "(string=? \"a\" \"a\" \"b\" \"a\")" `shouldEval` Bool False
          it "properly finds type errors"
            $ readRun "(string=? \"a\" \"a\" 1 \"a\" 2)"
            `shouldThrow` (== TypeMismatch "Expected string" (Integer 1))

      describe "comparisons"
        $ do
          it "compares two strings with <"
            $ "(string<? \"a\" \"a\")" `shouldEval` Bool False
          it "compares two strings with <="
            $ "(string<=? \"a\" \"a\")" `shouldEval` Bool True
          it "compares two strings with >"
            $ "(string>? \"a\" \"a\")" `shouldEval` Bool False
          it "compares two strings with >="
            $ "(string>=? \"a\" \"a\")" `shouldEval` Bool True
          it "compares two case sensitive strings with <"
            $ "(string<? \"A\" \"a\")" `shouldEval` Bool True
          it "compares two case sensitive with >"
            $ "(string>? \"hi\" \"hI\")" `shouldEval` Bool True

      describe "case insensitive"
        $ do
          -- again light on tests, bc we tested above
          -- and these should just sorta work... lol
          it "determines case insensitive equality"
            $ "(string-ci=? \"a\" \"A\")" `shouldEval` Bool True
          it "determines case insensitive comparison"
            $ "(string-ci<? \"B\" \"a\")" `shouldEval` Bool False

  describe "boolean comparisons"
    $ do
      it "compares many booleans"
        $ "(boolean=? #t #t (= 1 1) #t)" `shouldEval` Bool True
      it "compares unequal booleans"
        $ "(boolean=? #t #f)" `shouldEval` Bool False
