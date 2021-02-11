module Type.Types where

import Data.Data
import Data.Typeable
import Data.Text hiding (foldr1)


newtype TypeIdentifier = TypeId Text deriving (Eq, Data, Typeable)
instance Show TypeIdentifier where
  show (TypeId x) = unpack $ x

data BaseType
  = Unit
  | NamedType TypeIdentifier
  | IntType
  | StringType
  deriving (Show, Eq, Data, Typeable)

data Type
  = Base BaseType
  | LinearFunction Type Type
  | Sum Type Type
  | Product Type Type
  | Session SessionType
  | Any
  | Disposable
  deriving
    (Show, Data, Typeable)

instance Eq Type where
  (Base x) == (Base y) = x == y
  (LinearFunction x1 y1) == (LinearFunction x2 y2) = (x1 == x2) && (y1 == y2)
  (Sum x y) == (Sum x1 y1) = (x == x1) && (y == y1)
  (Product x y) == (Product x1 y1) = (x == x1) && (y == y1)
  (Session s) == (Session s1) = s == s1
  Any == _ = True
  _ == Any = True
  Disposable == x = isDisposable x
  x == Disposable = isDisposable x
  _ ==  _ = False

a -@ b = LinearFunction a b
a +: b = Sum a b
a *: b = Product a b
a !. b = Session $ Send a b
a ?. b = Session $ Recv a b

mkProduct :: (Foldable f) => f Type -> Type
mkProduct ts = foldr1 Product ts

mkSum :: (Foldable f) => f Type -> Type
mkSum ts = foldr1 Product ts

mkFunction ts = foldr1 LinearFunction ts

isDisposable :: Type -> Bool
isDisposable (Base Unit) = True
isDisposable (Base IntType) = True
isDisposable (Base StringType) = True
isDisposable (Product t ts) = (isDisposable t) && (isDisposable ts)
isDisposable _ = False

data SessionType
  = Send Type SessionType
  | Recv Type SessionType
  | SendEnd
  | RecvEnd deriving (Show, Eq, Data, Typeable)

dual :: SessionType -> SessionType
dual (Send t s) = (Recv t $ dual s)
dual (Recv t s) = (Send t $ dual s)
dual SendEnd = RecvEnd
dual RecvEnd = SendEnd

dualEq :: SessionType -> SessionType -> Bool
dualEq (Send t s) (Recv t' s') = (t==t') && (dualEq s s')
dualEq (Recv t s) (Send t' s') = (t==t') && (dualEq s s')
dualEq SendEnd RecvEnd = True
dualEq RecvEnd SendEnd = True
dualEq _ _ = False

sessionLast :: SessionType -> SessionType
sessionLast (Send _ ss) = sessionLast ss
sessionLast (Recv _ ss) = sessionLast ss
sessionLast s@(SendEnd) = s
sessionLast s@(RecvEnd) = s

sessionHead :: SessionType -> Type
sessionHead (Send t _) = t
sessionHead (Recv t _) = t
sessionHead s@(SendEnd) = Session s
sessionHead s@(RecvEnd) = Session s
