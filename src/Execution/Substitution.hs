{-# LANGUAGE AllowAmbiguousTypes #-}
module Execution.Substitution where

-- import GHC.Generics
-- import Data.Coerce
import Control.Monad.State.Lazy
-- import Data.Text hiding (zip, foldl', zipWith)
-- import Data.List
import Data.Maybe
import Data.Foldable
-- import Data.Functor.Const


import qualified Data.DList as DL
-- import Debug.Trace
-- import Util.Pretty

import AST.AST
import AST.Transformations

import Data.Generics.Fixplate


-- State Monad newtype wrapper, for unique names
newtype NameGen a = NameGen (State Integer a)
  deriving Functor via (State Integer)
  deriving Applicative via (State Integer)
  deriving Monad via (State Integer)
  deriving (MonadState Integer) via (State Integer)

-- Run sequence of name generation operations
execNameGen :: NameGen a -> a
execNameGen (NameGen x) = evalState x 0

-- Mangle a name with last generated unique name
useName :: NameGen (TermIdentifier -> TermIdentifier)
useName = (flip mangle) <$> get

-- Generate a new name to be used next
nextName :: NameGen ()
nextName = (modify (+1))

freshName :: TermIdentifier ->  NameGen (TermIdentifier)
freshName name = (mangle name) <$> get <* (modify (+1))


-- Alpha converts all bound variables to globally unique names
uniqueNames :: (LiftTransform t Term) => t -> t
uniqueNames x = execNameGen $ liftTransformM  rename x

-- Non-recursive transformation,
-- renames bound variables in a term
rename :: Attr Term a -> NameGen (Attr Term a)
rename term@(Fix (Ann (ann :: a) (Lambda nameTerm body))) = fromMaybe (return term) $ do
  (oldName, nameHole) <- getNameHole nameTerm
  return $ (\newName -> Fix $ Ann ann $ Lambda (nameHole newName) (substName oldName newName body)) <$>  (freshName oldName)

rename (Fix (Ann (ann :: a) (Case caseterm l r))) = do
  l' <- renameBranch l
  r' <- renameBranch r
  return $ Fix $ Ann ann (Case caseterm (l') (r'))
  where
    renameBranch :: CaseBranch (Attr Term a) -> NameGen (CaseBranch (Attr Term a))
    renameBranch branch@(CaseBranch nameTerm body) = fromMaybe (return branch) $ do
      (oldName, nameHole) <- getNameHole nameTerm
      return $ (\newName -> CaseBranch (nameHole newName) (substName oldName newName body)) <$> (freshName oldName)

rename term@(Fix (Ann (ann :: a) (Let ns vs body) )) = do
  (body', ns') <- foldl' (\m a -> m >>= flip renameLet a) (return (body, DL.empty)) ns
  return $ Fix $ Ann ann $ Let (DL.toList ns') vs body'

  where
    renameLet :: ((Attr Term a), DL.DList (Attr Term a)) -> (Attr Term a) -> NameGen ((Attr Term a), DL.DList (Attr Term a))
    renameLet (body, ns) nameTerm = fromMaybe (return (body, ns `DL.snoc` nameTerm)) $ do
      (oldName, nameHole) <- getNameHole nameTerm
      return $ (\newName -> ((substName oldName newName body), ns `DL.snoc` (nameHole newName))) <$> (freshName oldName)

rename x = return $ x

-- Extract the name, and a way to reconstruct it from the name term
getNameHole :: (Attr Term a) -> Maybe (TermIdentifier, (TermIdentifier -> (Attr Term a)))
getNameHole term = listToMaybe [(name, (\n -> hole $ Name' n annot)) | (Name' name annot, hole) <- contextList term]

-- recursive transformation,
-- replace all instances of a name in body with a new name
substName :: TermIdentifier -> TermIdentifier -> Attr Term a -> Attr Term a
substName oldName newName term = transform (substName' oldName newName) term
  where
    -- non-recursive transformation
    substName' :: TermIdentifier -> TermIdentifier -> Attr Term a -> Attr Term a
    substName' oldName newName (Id' name annot) | name == oldName = Id' newName annot
    substName' _ _ term = term

-- recursive transformation,
-- Beta substitution, assuming unique names
-- replace all instances of a name in body with a new Term (given a way to construct the new term with attributes)
-- Polymorphic over annotation type and Term subtype
{-
subst :: forall a sup sub t. (Functor sup, Functor sub,  Term :<: sup, sub :<: sup, LiftTransform t sup) => TermIdentifier -> (forall a. a -> (Attr sub a)) -> t -> t
subst oldName mkNew t = liftTransform (substTerm oldName mkNew) t
  where
    -- Non-recursive transformation for subst
    substTerm :: forall a. TermIdentifier -> (a -> (Attr sub a)) -> (Attr sup a) -> (Attr sup a)
    substTerm oldName newTerm t@(Fix (Ann (annot :: a) term)) =
      case (proj @ Term term) of
        (Just (Id name)) | name == oldName -> injectAttr $ newTerm annot
        _ -> t
-}
subst :: forall sub super. (Functor super, Term :<: super, sub :<: super) => (Fix super) -> (TermIdentifier, sub (Fix super)) -> (Fix super)
subst term (name, replTerm) = transform (subst' name replTerm) term
  where
    subst' :: TermIdentifier -> (sub (Fix super)) -> (Fix super) -> (Fix super)
    subst' name replTerm term = fromMaybe term $ do
      (Id termName) <- match term
      guard (termName == name)
      return $ inject replTerm



pattern Name' :: TermIdentifier -> a -> (Attr Term a)
pattern Name' name annot = Fix (Ann annot (Name name))

pattern Id' :: TermIdentifier -> a -> (Attr Term a)
pattern Id' name annot = Fix (Ann annot (Id name))
