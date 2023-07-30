{-# LANGUAGE OverloadedStrings #-}

module Kellisp.Parser where

import           Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import           Data.Void (Void)
import           Kellisp.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | The main parsing type, defined as a synonym for clean signatures
type Parser = Parsec Void T.Text

-- we are parsing Text (notably not strings),
-- and we have no custom error message component
-- | Parser for the special symbols that are permitted in identifiers
--  where the special identifiers are specified in R5RS
specialSymbol :: Parser Char
specialSymbol = oneOf ("!$%&*/:<=>?^_~" :: String) <?> "identifier symbol"

-- | Space consumer used for lexing
--  allowing multi-line comments with Racket's syntax
spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment ";") (L.skipBlockCommentNested "#|" "|#")

-- | Parses with the given parser, consuming trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Creates a parser for the given Text that will consume trailing whitespace
symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

-- | Parses an atom / identifier beginning with a character or special identifier
--  symbol, which may contain (but not begin with) numbers
parseAtom :: Parser LispVal
parseAtom = lexeme
  $ do
    first <- letterChar <|> specialSymbol
    rest <- many (alphaNumChar <|> specialSymbol)
    -- we read first and rest separately, since we cannot start with numbers
    -- we also pass the returned LispVal parser into lexeme, consuming whitespace
    return $ Atom $ T.pack (first:rest)

-- | Parses a reserved symbol, specifically `nil`, `#t`, or `#f`
parseReserved :: Parser LispVal
parseReserved = lexeme
  $ Nil <$ symbol "nil"
  <|> Bool True <$ symbol "#t"
  <|> Bool False <$ symbol "#f"

-- | Parses a string surrounded in double-quotes, considering escape characters
parseString :: Parser LispVal
parseString = lexeme
  $ do
    _ <- char '"'
    s <- manyTill L.charLiteral (char '"')
    return $ String $ T.pack s

-- | Parses a signed integer or signed double
parseSignedNumber :: Parser LispVal
parseSignedNumber = lexeme
  $ do
    n <- L.signed (return ()) L.scientific
    case floatingOrInteger n of
      Left d  -> return $ Double d
      Right i -> return $ Integer i

-- | Parses a list of LispVals surrounded by parentheses
parseList :: Parser LispVal
parseList = lexeme
  $ between (symbol "(") (symbol ")") (List <$> many parseExpr)

-- | Parses a sexpression
parseExpr :: Parser LispVal
parseExpr = parseReserved
  <|> parseAtom
  <|> parseString
  <|> parseSignedNumber
  <|> parseList

-- | Parses the entire sexpression to the end of file
-- currently, this ensures there aren't extra symbols after the sexpression
-- but this may have to be changed when file parsing is added
parseLispVal :: Parser LispVal
parseLispVal = between spaceConsumer eof parseExpr
