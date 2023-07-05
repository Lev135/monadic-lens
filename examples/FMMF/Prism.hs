module FMMF.Prism where
import Control.Lens (Choice(..), Profunctor(..))

type Prism s t a b =
  forall p f. (Choice p, Applicative f) =>
    p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure $ fmap bt) . right'

type PrismM m s t a b =
  forall p f. (Choice p, Applicative f) =>
    p a (f b) -> p s (f t)

prismM :: (b -> m t) -> (s -> m (Either t a)) -> PrismM m s t a b
prismM bt seta = _ . dimap _ _ . right'
