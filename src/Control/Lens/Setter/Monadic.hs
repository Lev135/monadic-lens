module Control.Lens.Setter.Monadic where

import Control.Lens.Internal.Setter (Settable(untainted))
import Control.Monad.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

type SetterM m s t a b =
  forall f. Settable f =>
    (a -> Compose m f b) -> s -> Compose m f t

type ASetterM m s t a b =
  (a -> Compose m Identity b) -> s -> Compose m Identity t

type ASetterM' m s a = ASetterM m s s a a

settingM :: Functor m => ((a -> m b) -> s -> m t) -> SetterM m s t a b
settingM setter f = Compose . fmap pure . setter (fmap untainted . getCompose . f)

overM :: Functor m => ASetterM m s t a b -> (a -> m b) -> s -> m t
overM l h = fmap runIdentity . getCompose . l (Compose . fmap Identity . h)

setM :: Functor m => ASetterM m s t a b -> m b -> s -> m t
setM l b = overM l (const b)
