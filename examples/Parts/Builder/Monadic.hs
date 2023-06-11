module Parts.Builder.Monadic where

import Control.Arrow (Kleisli(..))
import Data.Functor.Compose (Compose(..))
import Data.Tagged (Tagged(..))
{-
  type ABuilderM m t b = p b (Compose m f b) -> p t (Compose m f t)
    where
      p = Tagged
      f = Kleisli m b
-}
type ABuilderM m t b =
  Tagged b (Compose m (Kleisli m b) b) -> Tagged t (Compose m (Kleisli m b) t)


aBuilderM :: Applicative m => (b -> m t) -> ABuilderM m t b
aBuilderM bt _ = Tagged (Compose $ pure $ Kleisli bt)

buildM :: Monad m => ABuilderM m t b -> b -> m t
buildM l b = unpack (l (pack $ pure pure)) >>= ($ b)
  where
    pack = Tagged . Compose . fmap Kleisli
    unpack = fmap runKleisli . getCompose . unTagged
