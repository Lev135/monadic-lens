module Control.Lens.Setter.Monadic where
import Control.Monad.Identity
import Data.Functor.Compose (Compose(..))

type ASetterM m s t a b =
  (a -> Compose m Identity b) -> s -> Compose m Identity t

type ASetterM' m s a = ASetterM m s s a a

setsM :: Functor m => ((a -> m b) -> s -> m t) -> ASetterM m s t a b
setsM setter f = Compose . fmap Identity . setter (fmap runIdentity . getCompose . f)

overM :: Functor m => ASetterM m s t a b -> (a -> m b) -> s -> m t
overM l h = fmap runIdentity . getCompose . l (Compose . fmap Identity . h)

setM :: Functor m => ASetterM m s t a b -> m b -> s -> m t
setM l b = overM l (const b)
