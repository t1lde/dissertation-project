{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.Lexer where


import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx

import Data.Text

import           Parse.Base
import qualified Parse.Symbols as Sym

-- Skips spaces and comments
consumeSkipped :: Parser ()
consumeSkipped = Lx.space spaceConsumer lineComment blockComment
  where
    spaceConsumer = (some spaceChar) *> return ()
    lineComment  = Lx.skipLineComment Sym.lineCommentSymbol
    blockComment = Lx.skipBlockComment Sym.blockCommentStartSymbol Sym.blockCommentEndSymbol


-- Wraps parsers to include whitespace
lexeme :: Parser a -> Parser a
lexeme = Lx.lexeme consumeSkipped
symbol = Lx.symbol consumeSkipped

keyword k = chunk k *> space1

lexemeMany :: Parser a -> Parser [a]
lexemeMany p = consumeSkipped *> (many $ lexeme p)

-- Adds surrounding brackets to parser
inParens = between (symbol Sym.parenL) (symbol Sym.parenR)

-- adds keyword checking to parser
noKeywords :: Parser Text -> Parser Text
noKeywords p =
  (lexeme) (p >>= check)
  where
    check x =
      if x `elem` Sym.reservedKeywords
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x

parseIdentifier :: Parser Text
parseIdentifier =
  noKeywords $
  pack <$>
  (some idStart) <++>
  (many idString) <++>
  (many $ single '\'')
  where
    idStart :: Parser Char
    idStart = letterChar <|> (single '_')
    idString :: Parser Char
    idString = alphaNumChar <|> (single '_')


parseInteger :: Parser Integer
parseInteger = Lx.decimal

parseString :: Parser Text
parseString = pack <$> ((symbol Sym.quoteSymbol) *> manyTill Lx.charLiteral (symbol Sym.quoteSymbol))
