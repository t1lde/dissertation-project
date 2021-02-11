module AST.Transformations where

import GHC.Generics hiding (from, to)          -- Generic functor combinators
import Control.Monad

import Data.Void
import Data.Generics.Fixplate

import AST.AST

-- Data types a la carte typeclass, implements subtyping of functors
class sub :<: super where
  inj :: sub a -> super a
  proj :: super a -> Maybe (sub a)

-- Reflexive subtype
instance (Functor f) => f :<: f where
  inj = id
  proj = Just

-- Subtyping for Functor sums
-- Incoherent pragma used as the reflexive instance may overlap
instance {-# INCOHERENT #-}(Functor sub, Functor f) => sub :<: (sub :+: f) where
  inj = L1
  proj (L1 x) = Just x
  proj _ = Nothing

instance {-# INCOHERENT #-}
  ( Functor sub, Functor f, Functor super
  , sub :<: super) => sub :<: (f :+: super) where
  inj = R1 . inj
  proj (L1 _) = Nothing
  proj (R1 x) = proj x


-- Inject to Fix Functor
inject :: (f :<: g) => f (Fix g) -> Fix g
inject = Fix . inj

injectFix :: (f :<: g, Functor f, Functor g) => (Fix f) -> (Fix g)
injectFix = restructure inj

injectAttr :: (f :<: g, Functor f, Functor g) => (Attr f a) -> (Attr g a)
injectAttr = restructure (\(Ann a x) -> (Ann a (inj x)))


-- Project from fixed functor to a subtype
match :: (sub :<: super) => (Fix super) -> Maybe (sub (Fix super))
match = proj . unFix


-- Typeclass to lift to and from AST
class LiftTransform f g where
  liftTransform ::  (forall a. Attr g a -> Attr g a) -> f -> f
  liftTransformM :: (Monad m)  => (forall a. Attr g a -> m (Attr g a)) -> f -> m f

instance (Traversable f) =>  LiftTransform (Attr f a) f where
  liftTransform trans = transform trans
  liftTransformM transM = transformM transM

instance (Traversable f) => LiftTransform (Fix f) f where
  liftTransform trans x = forget $ transform trans (synthetise (const ()) x)
  liftTransformM trans x = forget <$> transformM trans (synthetise (const ()) x)
