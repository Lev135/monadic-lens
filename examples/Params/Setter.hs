module Params.Setter where

import Control.Monad.Identity
import Data.Coerce
import Params.Trans

type SetterM m s t a b = forall f. (SettableM f, Functor (f m), Join f) => (a -> f m b) -> s -> f m t

setterM :: ((a -> m b) -> s -> m t) -> SetterM m s t a b
setterM ambsmt amb = fromApp . ambsmt (toApp . amb)

type ASetterM m s t a b = (a -> IdentityT m b) -> s -> IdentityT m t

overM :: ASetterM m s t a b -> (a -> m b) -> s -> m t
overM = coerce

class SettableM f where
  fromApp :: m a -> f m a
  toApp   :: f m a -> m a

instance SettableM IdentityT where
  fromApp = coerce
  toApp = coerce

-- L.Setter is not a SetterM, since Setter does not allow effects
-- setterIsSetterM :: Monad m => L.Setter s t a b -> SetterM m s t a b
-- setterIsSetterM l = l
