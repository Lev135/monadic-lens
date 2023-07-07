module Params.Lens where

import qualified Control.Lens as L
import Params.Getter (GetterM, getPure)
import Params.Setter (SetterM)
import Params.Trans

type LensM m s t a b
  = forall f. (Functor (f m), Join f) =>
      (a -> f m b) -> s -> f m t

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter afb s = bindIn (setter s) . bindOut afb $ getter s

lensMIsSetterM :: Monad m => LensM m s t a b -> SetterM m s t a b
lensMIsSetterM l = l

lensMIsGetterM :: Monad m => LensM m s s a a -> GetterM m s a
lensMIsGetterM l = l

lensIsLensM :: L.Lens s t a b -> LensM m s t a b
lensIsLensM l = l

type LensMFwd m s t a b
  = forall f. (Functor (f m), JoinOuter f) =>
    (a -> f m b) -> s -> f m t

lensMFwd :: Monad m => (s -> m a) -> (s -> b -> t) -> LensMFwd m s t a b
lensMFwd getter setter afb s = fmap (setter s) . bindOut afb $ getter s

lensMFwdIsSetterM :: Monad m => LensMFwd m s t a b -> SetterM m s t a b
lensMFwdIsSetterM l = l

lensMFwdIsGetterM :: Monad m => LensMFwd m s s a a -> GetterM m s a
lensMFwdIsGetterM l = l

type LensMBwd m s t a b
  = forall f. (Functor (f m), JoinInner f) =>
    (a -> f m b) -> s -> f m t

lensMBwd :: Monad m => (s -> a) -> (s -> b -> m t) -> LensMBwd m s t a b
lensMBwd getter setter afb s = bindIn (setter s) . afb $ getter s

lensMBwdIsSetterM :: Monad m => LensMBwd m s t a b -> SetterM m s t a b
lensMBwdIsSetterM l = l

lensMBwdIsGetterM :: Monad m => LensMBwd m s s a a -> GetterM m s a
lensMBwdIsGetterM l = l

lensMBwdIsGetting :: Monad m => LensMBwd m s s a a -> L.Getter s a
lensMBwdIsGetting l = getPure l
