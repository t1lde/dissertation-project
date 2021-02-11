module Parse.Parse where

import Text.Megaparsec hiding (parse)

import Data.Void
import Data.List as L
import Data.Text

import Control.Monad

import AST.AST
import Type.Types

import Parse.Base
import Parse.AST
import Parse.Types
import Parse.TopLevel
import Parse.Lexer

class Parse a where
  parse :: Parser a
  parseFile :: String -> Text -> ParseResult a
  parseFile filename input = runParser (parse <* eof) filename input


instance Parse AST where
  parse = parseTypedTerm

instance Parse Type where
  parse = parseType

instance Parse Program where
  parse = Program <$> lexemeMany parse

instance Parse TopLevel where
  parse = parseTL


withTk p = match $ p
