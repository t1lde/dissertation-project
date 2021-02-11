module Execution.Runtime where

import Data.Data
import Data.Typeable
import Data.Text
import Data.List.NonEmpty
import Data.String
import Data.Functor.Const
import Data.Coerce
import Data.Maybe

import GHC.Generics
import Data.Generics.Fixplate
import Control.Monad.State.Lazy

import qualified AST.AST as AST
import AST.AST
import AST.Builtins
import AST.Transformations
import Execution.Substitution
import Util.Pretty

import Debug.Trace

newtype ChannelIdentifier = ChanId Integer
  deriving (Show, Eq, Ord )
  deriving Num via Integer

instance Pretty ChannelIdentifier where
  prettyShow (ChanId x) = "<" <> (pack $ show x) <> ">"

newtype Para a = Para (NonEmpty a)
  deriving (Eq, Show, Functor, Traversable, Foldable)

instance (Pretty a) => Pretty (Para a) where
  prettyShow (Para (x :| [])) = prettyShow x
  prettyShow (Para xs) = "(| " <> intercalate " || " (fmap prettyShow $ toList xs)  <> " |)"

data Value a = Value a
  deriving (Eq, Show, Functor, Traversable, Foldable)

instance (Pretty a) => Pretty (Value a) where
  prettyShow (Value v) = prettyShow v

data EvalContext a = EvalContext a
  deriving (Eq, Show, Functor, Traversable, Foldable)

instance (Pretty a) => Pretty (EvalContext a) where
  prettyShow (EvalContext ctx) = prettyShow ctx

-- Structure to handle partial application of builtin functions
data Arg a
  = EmptyArg
  | AppArg a
  deriving (Eq, Show, Functor, Traversable, Foldable)

instance (Pretty a) => Pretty (Arg a) where
  prettyShow EmptyArg = "_"
  prettyShow (AppArg t) = "arg"

data Partial a
  = BinaryFn BuiltInFn (Arg a) (Arg a)
  | UnaryFn  BuiltInFn (Arg a)
  deriving (Eq, Show, Functor, Traversable, Foldable)


instance (Pretty a) => Pretty (Partial a) where
  prettyShow (BinaryFn fn x y) = "(" <> (prettyShow fn) <> " " <> (prettyShow x) <> " " <> (prettyShow y) <> ")"
  prettyShow (UnaryFn fn x) = "(" <> (prettyShow fn) <> " " <> (prettyShow x) <> ")"


-- Runtime program representation - AST with Channels, Paralell terms, and Value/EvalContext tags
type Runtime = Fix (EvalContext
                    :+: Value
                    :+: Para
                    :+: (Const ChannelIdentifier)
                    :+: Partial
                    :+: Term)

-- Runtime environment managing state and paralell terms
data RuntimeEnv = Env {envNewName :: Integer, envPara :: [Runtime], envPrint :: Text}
type RunState = State RuntimeEnv

initEnv :: RuntimeEnv
initEnv = Env 0 [] ""

incChan :: RuntimeEnv -> RuntimeEnv
incChan (Env ch par prints) = Env (ch + 1) par prints

addPara :: Runtime -> RuntimeEnv -> RuntimeEnv
addPara term (Env n paras prints) = Env n (term : paras) prints

writePrint :: Text -> RuntimeEnv -> RuntimeEnv
writePrint t (Env n paras prints) = Env n paras (prints <> "\n" <> t)

-- Returns first value, adds second to environment
infixl 9 +|
(+|) :: Runtime -> Runtime -> RunState Runtime
parent +| child = do
  modify $ addPara child
  return parent

-- Generator for runtime channel-names
newChannel ::  RunState (Runtime)
newChannel = do
  env <- get
  modify incChan
  return $ (inject $ Const $ ChanId (envNewName env))

-- Generator for identity functions (used for print/dispose)
newId :: RunState (Runtime)
newId = do
  env <- get
  modify incChan
  let newName = mangle "x" $ envNewName env
  return $ (inject $ Lambda (inject $ Name newName) (inject $ Id newName))

stripTypes :: Fix Term -> Fix Term
stripTypes = transform stripTypes'
  where
    stripTypes' :: Fix Term -> Fix Term
    stripTypes' (Fix (Typed t _)) = t
    stripTypes' t = t

mkUnary :: BuiltInFn -> Runtime
mkUnary f  = inject $ UnaryFn f EmptyArg
mkBinary :: BuiltInFn -> Runtime
mkBinary f = inject $  BinaryFn f EmptyArg EmptyArg

partialise :: Runtime -> Runtime
partialise = transform partialise'
  where
    partialise' :: Runtime -> Runtime
    partialise' term = fromMaybe term (toPartial <$> (matchBuiltIn term))

    matchBuiltIn :: Runtime -> Maybe (BuiltInFn)
    matchBuiltIn term = do
      (BuiltIn fn) <- match term
      return fn

    toPartial :: BuiltInFn -> Runtime
    toPartial IntPlus = mkBinary IntPlus
    toPartial IntSub = mkBinary IntSub
    toPartial IntTimes = mkBinary IntTimes
    toPartial IntNeg = mkUnary IntNeg
    toPartial StringConcat = mkBinary StringConcat
    toPartial PrintVal = mkUnary PrintVal
    toPartial DisposeVal = mkUnary DisposeVal

toRuntime :: AST -> Runtime
toRuntime = (partialise . injectFix . stripTypes . forget)


testConcatTerm :: Runtime
testConcatTerm = inject $ BinaryFn StringConcat (AppArg $ inject $ StringL "testing ") (AppArg $ inject $ StringL "testing")

testConcatTerm' :: Runtime
testConcatTerm' = inject $ Value $ inject $ StringL "testing testing"
