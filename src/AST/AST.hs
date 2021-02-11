module AST.AST where

import Data.Hashable
import Data.Typeable
import Data.List.NonEmpty
import Data.Data
import Data.Text
import Data.String

import AST.Builtins

import GHC.Generics
import qualified Type.Types as Types
-- -- import Data.Functor.Classes
-- -- import Data.Functor.Const

import Data.Generics.Fixplate

newtype TermIdentifier = TermId {unwrapId :: Text}
  deriving (Eq, Ord, Generic, Data, Typeable)
  deriving Hashable via Text
  deriving IsString via Text


newtype IntLiteral = IntLiteral {unInt :: Integer}
  deriving (Eq, Ord, Generic, Data, Typeable)
  deriving Num via Integer
  deriving Show via Integer

newtype StringLiteral = StringLiteral {unString :: Text}
  deriving (Eq, Ord, Generic, Data, Typeable, Show)
  deriving IsString via Text
  deriving (Semigroup, Monoid) via Text

-- Mangles a name by inserting a show-able disambiguator with # as a separator
-- Assumption: the TermIdentifier must not contain a # (as governed by the parser grammar)
mangle :: (Show a) => TermIdentifier -> a -> TermIdentifier
mangle (TermId name) n = TermId ((pack $ show n) <> "#" <> name)

-- Unmangles a name to retrieve the original name
unmangle :: TermIdentifier -> TermIdentifier
unmangle (TermId name) = TermId $ snd $ breakOnEnd "#" name

instance Show TermIdentifier where
  show (TermId name) =  unpack name

-- Term pattern functor with explicit recursion replaced
-- with a polymorphic parameter
data Term a = UnitVal
  | IntL IntLiteral
  | StringL StringLiteral
  | BuiltIn BuiltInFn
  | Id TermIdentifier
  | Name TermIdentifier
  | Apply a a
  | Tuple (NonEmpty a)
  | Lambda a a
  | Let [a] a a
  | Case a (CaseBranch a) (CaseBranch a)
  | InL a
  | InR a
  | Fork a
  | Send a a
  | Recieve a
  | Wait a
  | Typed a Types.Type
  deriving (Eq, Data, Typeable, Functor, Traversable, Foldable, Show)

data CaseBranch a =
  CaseBranch a a deriving (Show, Eq, Data, Typeable, Functor, Traversable, Foldable)

-- AST offset span annotations, for type error reporting
newtype TermLoc = TermLoc {unLoc::(Int,Int)}
  deriving (Show, Data, Typeable)

-- Fixed point of Functor
type Fix = Mu
{-
newtype Fix f = Fix {unfix :: f (Fix f)}
  deriving (Data, Typeable, Show) via (Fix f)
-}
instance Semigroup TermLoc where
  (TermLoc (start,_)) <> (TermLoc (_,end)) = TermLoc (start,end)

-- AST as Functor product with annotations
--type AST = Fix (Term :*: (Const TermLoc))
type AST = Attr Term TermLoc
type ASTTerm = Term AST

-- Symmetric pattern synonyms for convenient pattern matching and constructors
pattern AST :: ASTTerm -> TermLoc -> AST
--pattern AST term loc = Fix (term :*: (Const loc))
pattern AST term loc = Fix (Ann loc term)
{-# COMPLETE AST #-}

pattern TypedName :: TermIdentifier -> Types.Type -> TermLoc -> TermLoc -> AST
pattern TypedName name ty locName locType = (AST (Typed (AST (Name name) locName) ty) locType)
