module Parse.Types where

import Text.Megaparsec

import Data.Data

import qualified Parse.Symbols as Sym
import Type.Types
import Parse.Base
import Parse.Lexer


-- Left recursive rules
parseType :: Parser Type
parseType = try parseLinearFunctionType
  <|> try parseSumType
  <|> try parseProductType
  <|> try parseSessionType
  <|> parseType'

-- Non left-recursive
parseType' :: Parser Type
parseType' = try parseBaseType
  <|> (inParens parseType)

parseBaseType :: Parser Type
parseBaseType = Base <$> parseBase

parseBase :: Parser BaseType
parseBase
  =   (Unit <$ (symbol Sym.unitTypeSymbol))
  <|> try (IntType <$ (symbol Sym.intTypeSymbol))
  <|> try (StringType <$ (symbol Sym.stringTypeSymbol))
  <|> (NamedType <$> parseTypeId)

parseTypeId :: Parser TypeIdentifier
parseTypeId = TypeId <$> parseIdentifier

-- Right Associative, Left Recursive
parseLinearFunctionType :: Parser Type
parseLinearFunctionType = LinearFunction
  <$> (parseType' <* (symbol Sym.linearFunctionTypeSymbol) )
  <*> parseType

parseSumType :: Parser Type
parseSumType = Sum
  <$> (parseType' <* (symbol Sym.sumTypeSymbol))
  <*> parseType

parseProductType :: Parser Type
parseProductType = Product
  <$> (parseType' <* (symbol Sym.productTypeSymbol))
  <*> parseType

parseSessionType :: Parser Type
parseSessionType = Session <$> parseSession


-- Non left-recursive session types

parseSession :: Parser SessionType
parseSession
  =   (inParens parseSession)
  <|> parseSendEndType
  <|> parseRecvEndType
  <|> (parseSendType <|> parseRecvType) <*> ((symbol Sym.sessionTypeConcatSymbol) *> parseSession)

parseSendType :: Parser (SessionType -> SessionType)
parseSendType = Send <$> ((symbol Sym.sendTypeSymbol) *> parseType)

parseRecvType :: Parser (SessionType -> SessionType)
parseRecvType = Recv <$> ((symbol Sym.recvTypeSymbol) *> parseType)

parseSendEndType :: Parser SessionType
parseSendEndType = SendEnd
  <$ (symbol Sym.sendEndTypeSymbol)

parseRecvEndType :: Parser SessionType
parseRecvEndType = RecvEnd
  <$ (symbol Sym.recvEndTypeSymbol)
