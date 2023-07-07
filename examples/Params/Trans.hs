{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Params.Trans where

import Control.Arrow (Kleisli(..))
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Kind
import Data.Tagged

class JoinOuter t where
  joinOut :: forall m a. Monad m => m (t m a) -> t m a

bindOut :: (JoinOuter t, Monad m) => (a -> t m b) -> m a -> t m b
bindOut k = joinOut . fmap k

class JoinInner t where
  joinIn :: forall m a. Monad m => t m (m a) -> t m a

bindIn :: (JoinInner t, Monad m, Functor (t m)) => (a -> m b) -> t m a -> t m b
bindIn k = joinIn . fmap k

type Join f = (JoinInner f, JoinOuter f)

class Fish p t where
  type ParP p (m :: Type -> Type) = (f :: Type -> Type -> Type) | f -> p
  fish :: forall m a b c. Monad m => (a -> m b) -> ParP p m b (t m c) -> ParP p m a (t m c)

-- IdentityT

instance JoinInner IdentityT where
  joinIn = IdentityT . join . runIdentityT

instance JoinOuter IdentityT where
  joinOut = IdentityT . (runIdentityT =<<)

-- Compose

instance Functor f => JoinInner (Compose f) where
  joinIn = Compose . fmap join . getCompose

-- Const2

newtype Const2 a m b
  = Const2 { getConst2 :: a }
  deriving (Functor)

instance Contravariant (Const2 a m) where
  contramap _ = Const2 . getConst2

instance JoinInner (Const2 a) where
  joinIn = phantom

-- ConstT

newtype ConstT a m b
  = ConstT { getConstT :: m a }
  deriving (Functor)

instance Contravariant (ConstT a m) where
  contramap _ = ConstT . getConstT

instance JoinInner (ConstT a) where
  joinIn = phantom

instance JoinOuter (ConstT a) where
  joinOut = ConstT . (getConstT =<<)

-- (->)

instance (JoinOuter t) => Fish (->) t where
  type ParP (->) _ = (->)
  fish amb btmc = bindOut btmc . amb

instance Fish (Tagged :: Type -> Type -> Type) t where
  type ParP Tagged _ = Tagged
  fish _ x = retag x

-- Kleisli

newtype KleisliT a m b
  = KleisliT { runKleisliT :: a -> m b }
  deriving (Functor)

deriving via (Kleisli m a) instance Applicative m => Applicative (KleisliT a m)
deriving via (Kleisli m a) instance Monad m => Monad (KleisliT a m)

instance JoinInner (KleisliT a) where
  joinIn = KleisliT . fmap join . runKleisliT
