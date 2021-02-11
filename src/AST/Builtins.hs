-- Maybe make builtins module instead?
module AST.Builtins where

import Data.Typeable
import Data.Data

import Data.Text


import Type.Types

-- AST
data BuiltInFn
  = IntPlus
  | IntSub
  | IntTimes
  | IntNeg
  | StringConcat
  | PrintVal
  | DisposeVal
  deriving (Eq, Data, Typeable, Show)


-- Types
typeBuiltIn :: BuiltInFn -> Type
typeBuiltIn IntPlus = Base IntType `LinearFunction` (Base IntType `LinearFunction` Base IntType)
typeBuiltIn IntSub = Base IntType `LinearFunction` (Base IntType `LinearFunction` Base IntType)
typeBuiltIn IntTimes = Base IntType `LinearFunction` (Base IntType `LinearFunction` Base IntType)
typeBuiltIn IntNeg = Base IntType `LinearFunction` Base IntType
typeBuiltIn StringConcat = Base StringType `LinearFunction` (Base StringType `LinearFunction` Base StringType)
typeBuiltIn PrintVal = Disposable `LinearFunction` (Any `LinearFunction` Any)
typeBuiltIn DisposeVal = Disposable `LinearFunction` (Any `LinearFunction` Any)

-- Parser Symbols
builtInPlus :: Text
builtInPlus = "plus"

builtInSub :: Text
builtInSub = "sub"

builtInTimes :: Text
builtInTimes = "times"

builtInNeg :: Text
builtInNeg = "neg"

builtInConcat :: Text
builtInConcat = "concat"

builtInPrint :: Text
builtInPrint = "print"

builtInDispose :: Text
builtInDispose = "dispose"

builtInKeywords = [builtInPlus, builtInSub, builtInTimes, builtInNeg, builtInConcat, builtInPrint, builtInDispose]
