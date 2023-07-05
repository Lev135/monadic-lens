{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fprint-potential-instances#-}
{-# HLINT ignore "Use =<<" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE GADTs          #-}
module FMMF.Lens where

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
  = forall f. (Functor f, RightModule m f, Distributive' m f) =>
      (a -> f b) -> s -> f t

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter afb s =
  joinR $ (setter s =<<) <$> distribute' (afb <$> getter s)

lensMIsSetterM :: Monad m => LensM m s t a b -> SetterM m s t a b
lensMIsSetterM l = l

lensMIsGetterM :: Monad m => LensM m s s a a -> GetterM m s a
lensMIsGetterM l = l
