{-# LANGUAGE OverloadedStrings #-}

module Kellisp.ParserSpec where

import           Kellisp.Types
import           Kellisp.Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

-- for now, parseLispVal and parseExpr are treated the same
-- since I haven't decided how to handle extra content
-- also, only parseLispVal is tested explicitly, since that
-- is the only "entry point", that all other functions help
spec :: Spec
spec = do
  describe "parsing atoms"
    $ do
      it "parses identifiers"
        $ parse parseLispVal "" "hi" `shouldParse` Atom "hi"
      it "parses single char identifiers"
        $ parse parseLispVal "" "h" `shouldParse` Atom "h"
      it "parses identifers with numbers"
        $ parse parseLispVal "" "h1" `shouldParse` Atom "h1"
      it "does not parse identifers starting with numbers"
        $ parse parseLispVal "" `shouldFailOn` "1h"
      it "removes trailing whitespace"
        $ parse parseLispVal "" "bye   " `shouldParse` Atom "bye"

  describe "parsing peculiar identifers"
    $ do
      it "parses identifiers with special characters: +"
        $ parse parseLispVal "" "+" `shouldParse` Atom "+"
      it "parses identifiers with special characters: -"
        $ parse parseLispVal "" "-" `shouldParse` Atom "-"
      it "parses identifiers with whitespace"
        $ parse parseLispVal "" "+    " `shouldParse` Atom "+"

  describe "parsing reserved"
    $ do
      it "parses nil as nil, not atom"
        $ parse parseLispVal "" "nil" `shouldParse` Nil
      it "parses #t" $ parse parseLispVal "" "#t" `shouldParse` Bool True
      it "parses #f" $ parse parseLispVal "" "#f" `shouldParse` Bool False
      it "removes trailing whitespace"
        $ parse parseLispVal "" "nil   " `shouldParse` Nil

  describe "parsing numbers"
    $ do
      describe "parsing integers"
        $ do
          it "parses positive integers"
            $ parse parseLispVal "" "123" `shouldParse` Integer 123
          it "parses negative integers"
            $ parse parseLispVal "" "-123" `shouldParse` Integer (-123)
          it "parses explicitly positive integers as integers, not atoms"
            $ parse parseLispVal "" "+123" `shouldParse` Integer 123
          it "removes trailing whitespace"
            $ parse parseLispVal "" "456   " `shouldParse` Integer 456

      describe "parsing doubles"
        $ do
          it "parses positive doubles"
            $ parse parseLispVal "" "123.45" `shouldParse` Double 123.45
          it "parses negative doubles"
            $ parse parseLispVal "" "-123.45" `shouldParse` Double (-123.45)
          it "parses explicitly positive doubles"
            $ parse parseLispVal "" "+123.45" `shouldParse` Double 123.45
          it "parses numbers ending in .0"
            $ parse parseLispVal "" "1.0" `shouldParse` Double 1.0

  describe "parsing lists"
    $ do
      it "parses contents of parens as list"
        $ parse parseLispVal "" "(hi mom)"
        `shouldParse` List [Atom "hi", Atom "mom"]
      it "parses heterogeneous lists"
        $ parse parseLispVal "" "(hi 123)"
        `shouldParse` List [Atom "hi", Integer 123]
      it "parses nested lists"
        $ parse parseLispVal "" "(1 2 (3 4))"
        `shouldParse` List [Integer 1, Integer 2, List [Integer 3, Integer 4]]
      it "parses instantly nested lists"
        $ parse parseLispVal "" "((1 2) (3 4))"
        `shouldParse` List
          [List [Integer 1, Integer 2], List [Integer 3, Integer 4]]
      it "fails on mismatched lists"
        $ parse parseLispVal "" `shouldFailOn` "(1 2 3"

  describe "parsing quote"
    $ do
      it "parses quote as list"
        $ parse parseLispVal "" "'x"
        `shouldParse` List [Atom "quote", Atom "x"]
      it "parses quotes lists as nested lists"
        $ parse parseLispVal "" "'(x y)"
        `shouldParse` List [Atom "quote", List [Atom "x", Atom "y"]]
      it "allows whitespace after quote"
        $ parse parseLispVal "" "'   x" `shouldParse` List [Atom "quote", Atom "x"]

  describe "parsing strings"
    $ do
      it "parses normal strings"
        $ parse parseLispVal "" "\"hi\"" `shouldParse` String "hi"
      it "parses escape characters"
        $ parse parseLispVal "" "\"hi\nmom\"" `shouldParse` String "hi\nmom"
      it "removes trailing whitespace (outside of quotes)"
        $ parse parseLispVal "" "\"bye  \"  " `shouldParse` String "bye  "
