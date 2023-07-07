module Params.Getter where

import qualified Control.Lens as L
import Data.Coerce
import Data.Functor.Contravariant
import Params.Trans

type GetterM m s a
  = forall f. (Contravariant (f m), Functor (f m), Join f) =>
    (a -> f m a) -> s -> f m s

type GettingM m r s a
  = (a -> ConstT r m a) -> s -> ConstT r m s

toM :: Monad m => (s -> m a) -> GetterM m s a
toM sma afa = phantom . joinOut . fmap afa . sma

viewM :: forall m s a. Applicative m => GettingM m a s a -> s -> m a
viewM l = getConstT . l (ConstT . pure)

getterIsGettingM :: L.Getter s a -> GettingM m a s a
getterIsGettingM l = l

-- actually is just pure 'Getting'
type GettingBwdM m r s a
  = (a -> Const2 r m a) -> s -> Const2 r m s

gettingPure :: GettingBwdM m r s a -> L.Getting r s a
gettingPure = coerce

getPure :: GettingBwdM m a s a -> L.Getter s a
getPure l = cloneGetter $ gettingPure l

cloneGetter :: L.Getting a s a -> L.Getter s a
cloneGetter l = L.to $ L.view l
