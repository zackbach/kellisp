{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.TypeConversionSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "conversions from string"
    $ do
      it "converts strings to integers"
        $ "(string->number \"123\")" `shouldEval` Integer 123
      it "converts strings to doubles"
        $ "(string->number \"123.45\")" `shouldEval` Double 123.45
      it "converts strings ending in .0"
        $ "(string->number \"123.0\")" `shouldEval` Double 123.0
      it "throws when non-numeric"
        $ readRun "(string->number \"a\")"
        `shouldThrow` (== TypeMismatch "Expected a number" (Atom "a"))

      it "converts strings to atoms"
        $ "(string->symbol \"a\")" `shouldEval` Atom "a"
      it "converts bad strings to atoms"
        $ "(string->symbol \"a b\")" `shouldEval` Atom "a b"

  describe "conversions to strings"
    $ do
      it "converts integers to strings"
        $ "(number->string 123)" `shouldEval` String "123"
      it "converts doubles to strings"
        $ "(number->string 123.45)" `shouldEval` String "123.45"

      it "converts between doubles and strings for small values"
        $ "(string->number (number->string 0.00000000001))" `shouldEval` Double 0.00000000001

      it "converts atoms to strings"
        $ "(symbol->string 'a)" `shouldEval` String "a"
      it "converts between atoms and strings"
        $ "(symbol->string (string->symbol \"hi mom\"))"
        `shouldEval` String "hi mom"
