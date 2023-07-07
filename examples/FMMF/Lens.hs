{-# LANGUAGE GADTs #-}
module FMMF.Lens where

import qualified Control.Lens as L
import FMMF.Getter
import FMMF.Setter

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter afb s = setter s <$> afb (getter s)

lensIsSetter :: Lens s t a b -> Setter s t a b
lensIsSetter l = l
lensIsGetter :: Lens s s a a -> Getter s a
lensIsGetter l = l

type LensM m s t a b
  = forall f. (Functor f, Module m f) =>
      (a -> f b) -> s -> f t

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter afb s = rjoin $ setter s <$> ljoin (afb <$> getter s)

lensMIsSetterM :: Monad m => LensM m s t a b -> SetterM m s t a b
lensMIsSetterM l = l

lensMIsGetterM :: Monad m => LensM m s s a a -> GetterM m s a
lensMIsGetterM l = l

lensMIsGettingMa :: Monad m => LensM m s s a a -> L.Getting (m a) s a
lensMIsGettingMa l = l

type LensMFwd m s t a b
  = forall f. (Functor f, LeftModule m f) =>
      (a -> f b) -> s -> f t

lensMFwd :: Monad m => (s -> m a) -> (s -> b -> t) -> LensMFwd m s t a b
lensMFwd getter setter afb s = setter s <$> ljoin (afb <$> getter s)

lensMFwdIsSetterM :: Monad m => LensMFwd m s t a b -> SetterM m s t a b
lensMFwdIsSetterM l = l

lensMFwdIsGetterM :: Monad m => LensMFwd m s s a a -> GetterM m s a
lensMFwdIsGetterM l = l

type LensMBwd m s t a b
  = forall f. (Functor f, RightModule m f) =>
      (a -> f b) -> s -> f t

lensMBwd :: Monad m => (s -> a) -> (s -> b -> m t) -> LensMBwd m s t a b
lensMBwd getter setter afb s = rjoin $ setter s <$> afb (getter s)

lensMBwdIsSetterM :: Monad m => LensMBwd m s t a b -> SetterM m s t a b
lensMBwdIsSetterM l = l

lensMBwdIsGetterM :: Monad m => LensMBwd m s s a a -> GetterM m s a
lensMBwdIsGetterM l = l

lensMBwdIsGetting :: Monad m => LensMBwd m s s a a -> L.Getting a s a
lensMBwdIsGetting l = l
