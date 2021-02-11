{-# LANGUAGE UndecidableInstances, QuantifiedConstraints, AllowAmbiguousTypes #-}
module Util.Pretty where

import qualified Data.List as DL hiding ((++))
import Data.Functor.Const

import qualified Type.Types as Type
import qualified AST.AST as AST
import AST.Builtins
import qualified Parse.Symbols as Sym
import qualified Parse.TopLevel as TL
import qualified Parse.Parse as P
import Parse.Base

import qualified Text.Megaparsec as MP
import Data.Void
import Data.Text
import qualified Data.Text as T

import Data.List.NonEmpty as NE
import Prelude hiding ((++))

import Data.Generics.Fixplate
import GHC.Generics

(++) = mappend

class Pretty a where
  parenShow :: a -> Text
  parenShow = ("("++) . (++")") . prettyShow
  prettyShow :: a -> Text

paren :: Text -> Text
paren x = "(" ++ x ++ ")"


test :: AST.AST -> Text
test x = prettyShow x
instance (Pretty a) => Pretty (a, Text) where
  prettyShow (rt, prints) = "Print Output:\n" <> prints <> "\nResult:\n" <> (prettyShow rt)
instance (Pretty a) => Pretty [a] where
  prettyShow xs = intercalate "\n" (fmap prettyShow xs)
instance (Pretty a, Pretty b) => Pretty (Either a b) where
  prettyShow (Left x) = prettyShow x
  prettyShow (Right y) = prettyShow y

instance Pretty ParseError where
  prettyShow = pack . MP.errorBundlePretty

instance Pretty AST.TermIdentifier where
  prettyShow n = AST.unwrapId $ AST.unmangle n

instance Pretty AST.IntLiteral where
  prettyShow n = pack $ show $ AST.unInt n

instance Pretty AST.StringLiteral where
  prettyShow s = "\"" ++ (AST.unString s) ++ "\""

instance Pretty BuiltInFn where
  prettyShow IntPlus = builtInPlus
  prettyShow IntSub = builtInSub
  prettyShow IntTimes = builtInTimes
  prettyShow IntNeg = builtInNeg
  prettyShow StringConcat = builtInConcat
  prettyShow PrintVal = builtInPrint
  prettyShow DisposeVal = builtInDispose

instance (Pretty x) => Pretty (Const x a) where
  prettyShow (Const x) = prettyShow x

instance (Pretty (f a), Pretty (g a)) => Pretty ((f :+: g) a) where
  prettyShow (L1 x) = prettyShow x
  prettyShow (R1 x) = prettyShow x

-- I dont know what this means or why it compiles...
instance (forall a. (Pretty a) => Pretty (f a)) => Pretty (Mu f) where
  prettyShow (Fix x) = prettyShow x

instance (Pretty (f a), Pretty b) => Pretty (Ann f b a) where
  prettyShow (Ann a x) = prettyShow x

instance Pretty a => Pretty (AST.Term a) where
  prettyShow n = case n of
    (AST.Id name) -> prettyShow name
    (AST.Name name) -> prettyShow name
    (AST.Apply x y ) -> paren $ (prettyShow x) ++ " " ++ (prettyShow y)
    (AST.Tuple xs ) -> paren $ intercalate ", " $ fmap prettyShow (toList xs)
    (AST.Lambda name body ) -> paren $ "lambda " ++ (prettyShow name) ++ "." ++ (prettyShow body)
    (AST.Let names values inTerm ) ->
      paren $ "let " ++ paren (intercalate "," (fmap prettyShow names)) ++ " = " ++  (prettyShow values) ++ " in " ++ (prettyShow inTerm)
    (AST.Case caseCheck caseL caseR ) -> paren $ "case "
      ++ (prettyShow caseCheck)
      ++ " of " ++ "{"
      ++ (prettyShowCase "inl " caseL) ++ ";"
      ++ (prettyShowCase "inr " caseR) ++ "}"
      where
        prettyShowCase :: Pretty a => Text -> AST.CaseBranch a -> Text
        prettyShowCase ~kw ~(AST.CaseBranch name res) = kw ++ (prettyShow name) ++ " |-> " ++ (prettyShow res)
    (AST.InL x ) -> paren $ "inl " ++ (prettyShow x)
    (AST.InR x ) -> paren $ "inr" ++ (prettyShow x)
    (AST.Fork x ) -> paren $ "fork " ++ (prettyShow x)
    (AST.Send x y ) -> paren $ "send " ++ (prettyShow x) ++ " "++ (prettyShow y)
    (AST.Recieve x ) -> paren $ ("recieve " ++ prettyShow x)
    (AST.Wait x ) -> paren $ ("wait " ++ prettyShow x)
    (AST.UnitVal ) -> "()"
    (AST.Typed x t ) -> (prettyShow x) ++ " " ++ Sym.typeAnnotationSymbol ++ " " ++ (prettyShow t)
    (AST.IntL n) -> prettyShow n
    (AST.StringL s) -> prettyShow s
    (AST.BuiltIn b) -> prettyShow b

instance Pretty Type.SessionType where
  prettyShow s = case s of
    (Type.Send t s') -> Sym.sendTypeSymbol ++ (prettyShow t) ++ (Sym.sessionTypeConcatSymbol) ++ (prettyShow s')
    (Type.Recv t s') -> Sym.recvTypeSymbol ++ (prettyShow t) ++ (Sym.sessionTypeConcatSymbol) ++ (prettyShow s')
    (Type.SendEnd) -> Sym.sendEndTypeSymbol
    (Type.RecvEnd) -> Sym.recvEndTypeSymbol

instance Pretty Type.BaseType where
  prettyShow b = case b of
    Type.Unit -> Sym.unitTypeSymbol
    (Type.NamedType name) -> (pack $ show name)
    (Type.IntType) -> Sym.intTypeSymbol
    (Type.StringType) -> Sym.stringTypeSymbol

instance Pretty Type.Type where
  prettyShow t = case t of
    (Type.Base b) -> (prettyShow b)
    (Type.Session s) -> (prettyShow s)
    (Type.LinearFunction x y) -> "(" ++ (prettyShow x) ++ " " ++ Sym.linearFunctionTypeSymbol ++ " " ++ (prettyShow y) ++ ")"
    (Type.Sum x y) -> (prettyShow x) ++ Sym.sumTypeSymbol ++ (prettyShow y)
    (Type.Product x y) -> (prettyShow x) ++ Sym.productTypeSymbol ++ (prettyShow y)
    (Type.Any) -> "t"
    (Type.Disposable) -> "Disposable"


instance Pretty TL.TopLevel where
  prettyShow t = case t of
    (TL.TypeDecl id' ty) -> Sym.keywordType ++ " " ++ (pack $ show id') ++ " " ++ Sym.typeDeclarationEqualsSymbol ++ " " ++ (prettyShow ty) ++ Sym.declEndSymbol
    (TL.Judgement ts tt) -> (intercalate (Sym.judgementSeparatorSymbol ++ " ") (fmap showTs ts)) ++ Sym.typeJudgementSymbol ++ " " ++ (prettyShow tt) ++ Sym.declEndSymbol
    (TL.TLTerm tt) -> (prettyShow tt) ++ Sym.declEndSymbol

showTs :: (Type.TypeIdentifier, Type.Type) -> Text
showTs (id', t) = (pack $ show id') ++ Sym.judgementNameSymbol ++ (prettyShow t)

instance Pretty TL.Program where
  prettyShow (TL.Program tls) = intercalate "\n" (fmap prettyShow tls)

instance Pretty AST.TermLoc where
  prettyShow (AST.TermLoc (start, end)) = "error at tokens "++ (pack $ show start) ++ "->" ++ (pack $ show end)
