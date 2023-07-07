{-# LANGUAGE MonoLocalBinds         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module FMMF.Getter where

import Control.Applicative
import qualified Control.Lens as L
import Control.Monad
-- import Data.Functor.Compose
import Control.Arrow (Kleisli(..))
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Kind

type Ap :: forall k. k -> (Type -> Type) -> Type -> Type
type family Ap t f = x where
  Ap (t :: (Type -> Type) -> Type -> Type) f = t f
  Ap (t :: Type -> Type) f = t

class Foo t where
  foo :: forall m a. m (Ap t m a) -> Ap t m a

{-
law> ljoin . fmap join ≡ ljoin join
law> ljoin . fmap pure ≡ ljoin pure ≡ id
law> ljoin . fmap (fmap f) ≡ fmap f . ljoin
-}
class (Monad m, Functor f) => LeftModule m f where
  ljoin :: m (f a) -> f a

class (Monad m, Functor f) => RightModule m f where
  rjoin :: f (m a) -> f a

-- Const functor

instance LeftModule m n => LeftModule m (Const (n x)) where
  ljoin = Const . ljoin . fmap getConst

instance (Monad m) => RightModule m (Const x) where
  rjoin = phantom


instance Monad m => RightModule m (Kleisli m a) where
  rjoin = Kleisli . fmap join . runKleisli

-- Compose functor

instance (LeftModule m n, Functor f) => LeftModule m (Compose n f) where
  ljoin = Compose . ljoin . fmap getCompose

instance (RightModule m n, Functor f) => RightModule m (Compose f n) where
  rjoin = Compose . fmap rjoin . getCompose

type Module m f = (LeftModule m f, RightModule m f)

-- Pure monad

instance {-# INCOHERENT #-} Monad m => LeftModule m m where
  ljoin = join

instance {-# INCOHERENT #-} Monad m => RightModule m m where
  rjoin = join

type Getter s a =
  forall f. (Contravariant f, Functor f) =>
    (a -> f a) -> s -> f s

type GetterM m s a =
  forall f. (Contravariant f, Functor f, Module m f) =>
    (a -> f a) -> s -> f s

to :: (s -> a) -> Getter s a
to h afa = phantom . afa . h

view :: Getter s a -> s -> a
view l = getConst . l Const

toM :: Functor m => (s -> m a) -> GetterM m s a
toM h afa = phantom . ljoin . fmap afa . h

viewM :: forall m s a. Monad m => GetterM m s a -> s -> m a
viewM l = getConst . l (Const . pure) -- L.views l pure

getterMIsGettingM :: forall m s a. Monad m => GetterM m s a -> L.Getting (m a) s a
getterMIsGettingM l = l

{-
(s -> m a) ≅ GetterM m s a:

  toM   :: (s -> m a) -> GetterM m s a
  viewM :: GetterM m s a -> (s -> m a)

laws:
  viewM . toM ≡ id
  toM . viewM ≡ asGetterM
  toM (f >=> g) ≡ toM f . toM g
  viewM (lf . lg) ≡ viewM lf >=> viewM lg
-}
