module Parse.TopLevel where

import Text.Megaparsec hiding (parse)

import Type.Types
import AST.AST
import Parse.Base
import Parse.Types(parseTypeId, parseType)
import Parse.AST(parseTypedTerm)
import Parse.Lexer

import qualified Parse.Symbols as Sym


newtype Program = Program {unProgram :: [TopLevel]}

mapP :: (AST -> AST) -> Program -> Program
mapP f (Program xs) = Program $ mapTL f <$> xs

programTerms :: [TopLevel] -> [AST]
programTerms ([]) = []
programTerms ((TLTerm t) : xs) = t : programTerms xs
programTerms ((Judgement _ t) : xs) = t : programTerms xs
programTerms (_:xs) = programTerms xs

data TopLevel = TLTerm AST
  | TypeDecl TypeIdentifier Type
  | Judgement [(TypeIdentifier, Type)] AST

mapTL :: (AST -> AST) -> TopLevel -> TopLevel
mapTL f (TLTerm t) = (TLTerm (f t))
mapTL f (Judgement x t) = Judgement x (f t)
mapTL _ tl = tl

parseTL :: Parser TopLevel
parseTL = (TLTerm <$> parseTypedTerm <* (symbol Sym.declEndSymbol))
  <|> parseDecl
  <|> parseJudgement

parseDecl :: Parser TopLevel
parseDecl = TypeDecl
  <$> ((symbol Sym.keywordType) *> parseTypeId)
  <*> ((symbol Sym.typeDeclarationEqualsSymbol) *> (parseType))
  <*  (symbol Sym.declEndSymbol)

parseJudgement :: Parser TopLevel
parseJudgement = Judgement
  <$> sepBy parseJudge (symbol Sym.judgementSeparatorSymbol)
  <*> ((symbol Sym.typeJudgementSymbol) *> parseTypedTerm)
  <* (symbol Sym.declEndSymbol)
  where
    parseJudge = (,)
      <$> (parseTypeId <* (symbol Sym.judgementNameSymbol))
      <*> (parseType)
