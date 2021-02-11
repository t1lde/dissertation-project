module Type.Checker where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.List.NonEmpty hiding (filter, length)
import Data.List as DL
import Data.Maybe

import qualified Data.HashMap as HM

import AST.AST
import AST.Builtins
import Type.Types
import qualified AST.AST as AST
import qualified Type.Types as Types
import Type.Error

import Util.Pretty

-- This could be Maybe Types.Type but this is more explicit
data BoundType
  = Unused Types.Type
  | Used
  deriving (Show, Eq)

unused (Unused _) = True
unused _ = False

type Context = HM.Map (AST.TermIdentifier) BoundType

-- Unapologetic abuse of notation
(|-) :: ((TypeCheck Context) ,(AST.TermIdentifier, Types.Type)) -> (TypeCheck a) -> TypeCheck a
(cx, (name, t)) |- chk = do
  c <- cx
  put $ HM.insert (name) (Unused t) c
  chk

type TypeCheck = ExceptT TypeError (State Context)


runChecker :: AST -> (Either TypeError Type, Context)
runChecker tt =  runState (runExceptT $ checkAST tt) mempty


-- Basic checker to catch and attach AST position to
type TypeCondition a = TypeCheck (Either ErrInfo a)

-- Handles basic error, throwing error with AST position
checkedAt :: TypeCondition a -> AST -> TypeCheck a
checkedAt chk (AST _ loc) = chk `checkedAtLoc` loc

checkedAtLoc :: TypeCondition a -> TermLoc -> TypeCheck a
checkedAtLoc chk loc = do
  e <- chk
  case e of
    (Left e') -> e' `thrownAtLoc` loc
    (Right x)  -> return x


-- Throw error at position of term
thrownAt :: ErrInfo -> AST -> TypeCheck a
thrownAt e (AST _ loc) = thrownAtLoc e loc

thrownAtLoc :: ErrInfo -> TermLoc -> TypeCheck a
thrownAtLoc e loc = throwError (TypeError loc e)

-- Signal Ok for a given check
ok :: a -> TypeCondition a
ok a = return (Right a)


-- Signal a basic error for a given checker (to add AST position later)
err :: ErrInfo -> TypeCondition a
err e = return (Left e)

-- Returns the current type context
ctx :: TypeCheck Context
ctx = get

-- Checks whether the current type context is empty
namesUsed :: [TermIdentifier] -> TypeCondition ()
namesUsed ns = do
  c <- ctx
  let c' = HM.keys $ HM.filter unused c
  case (c' `intersect` ns) of
    [] -> ok ()
    (x:xs) -> err (NotUsed (x:|xs))

matchTypes :: Types.Type -> Types.Type -> TypeCondition ()
matchTypes  Any _ = ok ()
matchTypes  _ Any = ok ()
matchTypes actual Disposable | (isDisposable actual) = ok ()
matchTypes actual Disposable | otherwise = err $ CannotDispose actual
matchTypes actual expected
  | actual == expected = ok ()
  | otherwise = err $ Mismatch actual expected

matchSum :: Types.Type -> Types.Type -> TypeCondition ()
matchSum (Sum actualL actualR) (Sum expectedL expectedR) | (actualL == expectedL) && (actualR == expectedR) = ok ()
matchSum actual expected = err $ Mismatch actual expected

-- Check that a type is a session type, returning the session type
checkSession :: Types.Type -> TypeCondition SessionType
checkSession (Session st) = ok st
checkSession t = err $ NotSession t

 -- Check that a type is a function type, returning the argument and body types
checkFunction :: Types.Type -> Types.Type -> TypeCondition (Types.Type, Types.Type)
checkFunction (LinearFunction targ tbody) _ = ok (targ, tbody)
checkFunction t arg = err $ NotFunction t arg

-- Lookup type of variable in context
lookupId :: AST.TermIdentifier -> TypeCheck (Maybe BoundType)
lookupId name = do
  c <- ctx
  return $ HM.lookup name c

-- Consume variable in type context
consumeName :: AST.TermIdentifier -> TypeCheck ()
consumeName name =
  modify $ HM.adjust (const Used) name

-- Construct the product type of a tuple term
checkTuple :: (NonEmpty AST) -> TypeCheck Types.Type
checkTuple (x :| []) = checkAST x
checkTuple (xs) = do
  txs <- mapM checkAST xs
  return $ mkProduct txs

-- Check that a term is a name, return it with its annotated type
checkName :: AST -> TypeCheck (AST.TermIdentifier, Types.Type)
checkName (TypedName name ty _ _) = return (name, ty)
checkName (AST (Name name) loc) = (UnknownType name) `thrownAtLoc` loc
checkName term = NotName `thrownAt` term

checkAST :: AST -> TypeCheck Types.Type
checkAST (AST term loc) = checkTerm term loc

checkTerm :: ASTTerm -> TermLoc -> TypeCheck Types.Type
checkTerm (Typed t _ ) _ = checkAST t
checkTerm (Id name) loc = do
  t <- lookupId name
  consumeName name
  case t of
    (Nothing) -> (NotInScope name) `thrownAtLoc` loc
    (Just (Unused t')) -> return t'
    (Just Used) -> (Reused name) `thrownAtLoc` loc

checkTerm (Apply f app) _ = do -- TApp
  tf <- checkAST f
  tapp <- checkAST app
  (targ, tbody) <- (checkFunction tf tapp) `checkedAt` f
  (matchTypes tapp targ) `checkedAt` app
  return tbody

checkTerm (Lambda (TypedName name t locName _) body) _ = do
  tbody <- (ctx,(name,t)) |- checkAST body
  namesUsed [name] `checkedAtLoc` locName
  return $ t -@ tbody
checkTerm (Lambda (AST (Name name) loc) _) _ = (UnknownType name) `thrownAtLoc` loc
checkTerm (Lambda term _ ) _ = (NotName) `thrownAt` term

checkTerm (UnitVal) _  = return $ Base Unit -- TUnit

checkTerm (IntL _) _ = return $ Base IntType
checkTerm (StringL _) _ = return $ Base StringType

checkTerm (BuiltIn b) _ = return $ typeBuiltIn b

checkTerm (Tuple ts ) _ = checkTuple ts

checkTerm (Let xs vs body) loc = do
  (names, namesType) <- checkNames xs
  checkConflict names `checkedAtLoc` loc
  tvs <- checkAST vs
  tbody <- checkAST body
  (namesUsed names) `checkedAtLoc` loc
  (matchTypes tvs namesType) `checkedAt` vs
  return tbody
  where
    checkNames :: [AST] -> TypeCheck ([AST.TermIdentifier], Types.Type)
    checkNames ts = do
      (nameTypes) <- (mapM checkName ts)
      sequence_ $ fmap (\x -> (ctx,x)|- (return ())) nameTypes
      let (names, types) = DL.unzip nameTypes
      return $ (names, (mkProduct types))
    checkConflict :: [TermIdentifier] -> TypeCondition ()
    checkConflict xs = case getDups xs of
      [] -> ok ()
      (x:xs) -> err $ (NameConflict $ x)
    getDups xs = fmap DL.head (DL.filter (\x -> length x > 1) (DL.group (DL.sort xs)))

checkTerm (Case chk l r) _ = do
  tc <- checkAST chk
  before_ctx <- ctx
  (tNameL, tResL) <- typeBranch l
  put before_ctx -- Reset the context after the branch, so that it is retained in the other branch
  (tNameR, tResR) <- typeBranch r
  matchSum tc (tNameL +: tNameR) `checkedAt` chk
  matchTypes tResL tResR `checkedAt` chk
  return tResL
  where
    typeBranch :: AST.CaseBranch AST -> TypeCheck (Types.Type, Types.Type)
    typeBranch (CaseBranch name body) = do
      (n, tn) <- checkName name
      tbody <- (ctx, (n, tn)) |- checkAST body
      namesUsed [n] `checkedAt` name
      return (tn, tbody)

checkTerm (InL t) _ = (+: (Any)) <$> (checkAST t)

checkTerm (InR t) _ = ((Any) +:) <$> (checkAST t)

checkTerm (Fork t) _ = do
  tt <- (checkAST t)
  checkForkFn tt
  where
    checkForkFn :: Types.Type -> TypeCheck Types.Type
    checkForkFn (LinearFunction (Session s) (Session SendEnd)) = return $ Session $ dual s
    checkForkFn ft = (NotFork ft) `thrownAt` t

checkTerm (AST.Send val session ) _ = do
  tval <- checkAST val
  tsession <- checkAST session
  (tHead, tTail) <- ((checkSession tsession) `checkedAt` session) >>= checkSend
  (matchTypes tval tHead) `checkedAt` val
  return $ Session tTail
  where
    checkSend :: Types.SessionType -> TypeCheck (Types.Type, Types.SessionType)
    checkSend (Types.Send t s) = return $ (t, s)
    checkSend r@(Types.Recv _ _) = (WrongSide r) `thrownAt` session
    checkSend s = (NoTail s) `thrownAt` session

checkTerm (AST.Recieve session) _ = do
  (tHead, tTail) <- (checkAST session >>= (checkSession)) `checkedAt` session >>= checkRecv
  return $ tHead *: (Session tTail)
  where
    checkRecv :: Types.SessionType -> TypeCheck (Types.Type, Types.SessionType)
    checkRecv (Types.Recv t s) = return (t, s)
    checkRecv s@(Types.Send _ _) =  (WrongSide s) `thrownAt` session
    checkRecv s = (NoTail s) `thrownAt` session

checkTerm (Wait t ) _ = do
  tt <- checkAST t
  (checkSession tt) `checkedAt` t >> matchTypes tt (Session RecvEnd)
  return $ Base Unit

checkTerm (Name _) loc = Malformed `thrownAtLoc` loc
