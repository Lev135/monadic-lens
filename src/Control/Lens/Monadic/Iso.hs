module Control.Lens.Monadic.Iso where

import Control.Lens (Profunctor(..), getting)
import Control.Lens.Monadic.Getter (viewM)
import Control.Lens.Monadic.Lens (FunctorM(..))
import Control.Lens.Monadic.Prism (ProfunctorM(fish), buildM)
import Data.Functor (($>))
import Data.Functor.Compose (Compose(..))

type IsoM m s t a b =
  forall p f. (ProfunctorM m p, FunctorM m f) =>
    p a (Compose m f b) -> p s (Compose m f t)

type IsoM' m s a = IsoM m s s a a

isoM :: Monad m => (s -> m a) -> (b -> m t) -> IsoM m s t a b
isoM sma bmt = rmap Compose . fish sma . rmap (fmapM bmt . getCompose)

viewIsoM :: Monad m => IsoM m s t a b -> s -> m a
viewIsoM l = viewM $ getting l

rviewIsoM :: Monad m => IsoM m s t a b -> b -> m t
rviewIsoM l = buildM l

acting :: Monad m => (s -> m ()) -> (s -> m ()) -> IsoM' m s s
acting fwd bwd = isoM (\s -> fwd s $> s) (\s -> bwd s $> s)

from :: Monad m => IsoM m s t a b -> IsoM m b a t s
from l = isoM (rviewIsoM l) (viewIsoM l)
