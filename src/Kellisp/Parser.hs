{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Parser where

import Data.Text qualified as T
import Data.Void (Void)
import Kellisp.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- | The main parsing type, defined as a synonym for clean signatures
type Parser = Parsec Void T.Text

-- we are parsing Text (notably not strings),
-- and we have no custom error message component

-- figure out parsing comments: can be part of lexer?
-- check out Text.Megaparsec.Char.Lexer `space`

-- note: use label aka <?> to make "expected blah blah" messages more readable
-- can also use hidden to remove noise from error messages (common with whitespace)

-- you can use L.charLiteral: experiment to see how that actually works
-- for numbers, try L.decimal, L.float, L.signed, etc

-- there are some utilities for parsing expressions in parser-combinators too,
-- where operator precedence is defined, etc. for now, i dont think that's needed

-- use takeWhileP, takeWhile1P, as fast choices when working with text (avoids a pack from some)
