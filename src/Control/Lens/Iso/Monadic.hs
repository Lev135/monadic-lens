module Control.Lens.Iso.Monadic where

import Control.Lens (Profunctor(..), getting)
import Control.Lens.Getter.Monadic (viewM)
import Control.Lens.Lens.Monadic (FunctorM(..))
import Control.Lens.Prism.Monadic (ProfunctorM(fish), buildM)
import Data.Functor.Compose (Compose(..))

type IsoM m s t a b =
  forall p f. (ProfunctorM m p, FunctorM m f) =>
    p a (Compose m f b) -> p s (Compose m f t)

isoM :: Monad m => (s -> m a) -> (b -> m t) -> IsoM m s t a b
isoM sma bmt = rmap Compose . fish sma . rmap (fmapM bmt . getCompose)

viewIsoM :: Monad m => IsoM m s t a b -> s -> m a
viewIsoM l = viewM $ getting l

rviewIsoM :: Monad m => IsoM m s t a b -> b -> m t
rviewIsoM l = buildM l

from :: Monad m => IsoM m s t a b -> IsoM m b a t s
from l = isoM (rviewIsoM l) (viewIsoM l)
