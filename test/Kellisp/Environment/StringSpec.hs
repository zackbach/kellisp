{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Environment.StringSpec where

import           Kellisp.SpecUtils

spec :: Spec
spec = do
  describe "string-append"
    $ do
      it "appends two strings"
        $ "(string-append \"hi\" \"mom\")" `shouldEval` String "himom"
      it "appends one string" $ "(string-append \"a\")" `shouldEval` String "a"
      it "appends zero strings" $ "(string-append)" `shouldEval` String ""
      it "appends many strings"
        $ "(string-append \"a\" \"b\" \"\" \"c\")" `shouldEval` String "abc"

  describe "string-length"
    $ do
      it "gets the length" $ "(string-length \"yass\")" `shouldEval` Integer 4
      it "works for empty string"
        $ "(string-length \"\")" `shouldEval` Integer 0

  -- for now, this returns a singleton string and NOT a character
  -- since characters currently do not exist
  describe "string-ref"
    $ do
      it "indexes starting at 0"
        $ "(string-ref \"hi\" 0)" `shouldEval` String "h"
      it "works in range" $ "(string-ref \"hi\" 1)" `shouldEval` String "i"
      it "throws out of bounds when indexing with length"
        $ readRun "(string-ref \"hi\" 2)" `shouldThrow` (== IndexOOB 2 0 1)
      it "throws when indexing with negative number"
        $ readRun "(string-ref \"hi\" -1)" `shouldThrow` (== IndexOOB (-1) 0 1)

  describe "substring"
    $ do
      it "can take zero-length substring"
        $ "(substring \"hi\" 0 0)" `shouldEval` String ""
      it "can take one-length substring"
        $ "(substring \"hi\" 0 1)" `shouldEval` String "h"
      it "can accept length as second int argument"
        $ "(substring \"hi\" 0 2)" `shouldEval` String "hi"
      it "throws if large second int argument is too large"
        $ readRun "(substring \"hi\" 0 3)" `shouldThrow` (== IndexOOB 3 0 2)
      -- there are probably more tests I could write here but this should be ok
      it "throws if second int argument is less than first"
        $ readRun "(substring \"hi\" 1 0)" `shouldThrow` (== IndexOOB 0 1 2)
