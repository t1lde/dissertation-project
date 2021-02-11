{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parse.AST where


import qualified Data.List                  as L
import qualified Data.List.NonEmpty         as NE
import           Data.Text hiding (some)
import           Data.Void

import           Control.Monad

import           Text.Megaparsec hiding (parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lx
import qualified Control.Monad.Combinators.NonEmpty as NE

import AST.AST
import AST.Builtins

import Type.Types hiding (Send)
import Parse.Base
import Parse.Lexer
import Parse.Types
import qualified Parse.Symbols as Sym

-- Adds annotation to Term parser
withLoc :: Parser ASTTerm -> Parser AST
withLoc p = do
  start <- getOffset
  term <- p
  end <- getOffset
  return $ AST term (TermLoc (start, end))

-- Wrap a parser with optional type annotation
withType :: Parser AST -> Parser ASTTerm
withType p = mkType <$> p <*> parseTypeAnnotation
  where
    mkType :: AST -> Maybe Type -> ASTTerm
    mkType (AST t _) (Nothing) = t
    mkType t (Just ty) = Typed t ty

-- Attach type annotation, with its own location annotation
withTypeLoc :: Parser ASTTerm -> Parser AST
withTypeLoc = withLoc . withType . withLoc

-- Potentially Left Recursive Terms
parseTypedTerm :: Parser AST
parseTypedTerm = (try parseApply) <|> parseTypedTerm'

-- Non Left Recursive Rules
parseTypedTerm' :: Parser AST
parseTypedTerm' = (try $ inParens parseTypedTerm) <|> (withTypeLoc $ parseTerm)
  --withLoc ((try $ inParens parseTypedTerm) <|> (withType parseTerm))

-- Non left recursive, untyped
parseTerm :: Parser ASTTerm
parseTerm = try parseUnitVal
  <|> parseStringLiteral
  <|> parseIntLiteral
  <|> try parseTuple
  <|> try parseLet
  <|> try parseLambda
  <|> try parseCase
  <|> try parseInL
  <|> try parseInR
  <|> try parseFork
  <|> try parseSend
  <|> try parseRecieve
  <|> try parseWait
  <|> try parseBuiltIn
  <|> parseId

parseId :: Parser ASTTerm
parseId =  Id <$> try (TermId <$> parseIdentifier)

-- Parse name with type and location annotation
parseName :: Parser AST
parseName = withTypeLoc $ Name <$> try (TermId <$> parseIdentifier)

parseUnitVal :: Parser ASTTerm
parseUnitVal =  UnitVal <$ (symbol Sym.unitTermSymbol)

parseStringLiteral :: Parser ASTTerm
parseStringLiteral = (StringL . StringLiteral) <$> parseString

parseIntLiteral :: Parser ASTTerm
parseIntLiteral = (IntL . IntLiteral) <$> parseInteger

parseBuiltIn :: Parser ASTTerm
parseBuiltIn = BuiltIn <$>
  ((IntPlus <$ (keyword builtInPlus))
  <|> (IntSub <$ (keyword builtInSub))
  <|> (IntTimes <$ (keyword builtInTimes))
  <|> (IntNeg <$ (keyword builtInNeg))
  <|> (StringConcat <$ (keyword builtInConcat))
  <|> (PrintVal <$ (keyword builtInPrint))
  <|> (DisposeVal <$ (keyword builtInDispose)))


-- No type annotation for parseApply itself, type annotation parsed in the bracketed term
parseApply :: Parser AST
parseApply = (L.foldl1' mkApply) <$> (some parseTypedTerm')
  where
    mkApply :: AST -> AST -> AST
    mkApply x@(AST _ locx) y@(AST _ locy) = AST (Apply x y) (locx <> locy)

parseTuple :: Parser ASTTerm
parseTuple =  Tuple <$> inParens ( parseTypedTerm `NE.sepBy1` (symbol Sym.tupleSeparatorSymbol))

parseLambda :: Parser ASTTerm
parseLambda =  Lambda <$> (lambdaSymbol *> parseName)
  <*> ((symbol Sym.lambdaDotSymbol) *> parseTypedTerm)
  where
    lambdaSymbol = keyword Sym.lambdaSymbol <|> keyword Sym.keywordLambda

parseLet :: Parser ASTTerm
parseLet =  Let
  <$> ((keyword Sym.keywordLet) *> letNamesTuple)
  <*> ((symbol Sym.equalsSymbol) *> parseTypedTerm)
  <*> ((keyword Sym.keywordIn) *>  parseTypedTerm)
  where
    letNamesTuple = try (inParens parseNames) <|> parseNames
    parseNames = (parseName `sepBy` (symbol Sym.tupleSeparatorSymbol))

parseCase :: Parser ASTTerm
parseCase =
   Case
  <$> ((keyword Sym.keywordCase) *> parseTypedTerm <* (keyword Sym.keywordOf))
  <*> ((symbol Sym.caseBracketL) *> parseCaseL <* (symbol Sym.caseSeparatorSymbol))
  <*> (parseCaseR <* (symbol Sym.caseBracketR))
  where
    parseCaseL = caseBranch (symbol Sym.keywordInL)
    parseCaseR = caseBranch (symbol Sym.keywordInR)
    caseBranch kw = CaseBranch
      <$> (kw *> parseName)
      <*> ((symbol Sym.caseResultSymbol) *> parseTypedTerm)

parseInL :: Parser ASTTerm
parseInL =  InL <$> ((keyword Sym.keywordInL) *> parseTypedTerm)

parseInR :: Parser ASTTerm
parseInR =  InR <$> ((keyword Sym.keywordInR) *> parseTypedTerm)

parseFork :: Parser ASTTerm
parseFork =  Fork <$> ((keyword Sym.keywordFork) *> parseTypedTerm)

parseSend :: Parser ASTTerm
parseSend =  Send <$> ((keyword Sym.keywordSend) *> parseTypedTerm') <*> parseTypedTerm'

parseRecieve :: Parser ASTTerm
parseRecieve = Recieve <$> ((keyword Sym.keywordRecieve) *> parseTypedTerm)

parseWait :: Parser ASTTerm
parseWait =  Wait <$> ((keyword Sym.keywordWait) *> parseTypedTerm)

parseTypeAnnotation :: Parser (Maybe Type)
parseTypeAnnotation = optional ((symbol Sym.typeAnnotationSymbol) *> parseType) <?> "type annotation"
