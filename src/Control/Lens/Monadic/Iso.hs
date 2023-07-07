{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Control.Lens.Monadic.Iso where

import Control.Lens (Profunctor(..), getting)
import Control.Lens.Monadic.Getter (viewM)
import Control.Lens.Monadic.Internal
import Control.Lens.Monadic.Prism
import Data.Functor (($>))

type IsoM m s t a b =
  forall k (p :: k) f. (Profunctor (ParP p m), Functor (f m), JoinInner f, Fish p f) =>
    ParP p m a (f m b) -> ParP p m s (f m t)

type IsoM' m s a = IsoM m s s a a

isoM :: Monad m => (s -> m a) -> (b -> m t) -> IsoM m s t a b
isoM sma bmt = fish sma . rmap (bindIn bmt)

viewIsoM :: Monad m => IsoM m s t a b -> s -> m a
viewIsoM l = viewM $ getting l

rviewIsoM :: Monad m => IsoM m s t a b -> b -> m t
rviewIsoM l = buildM l

acting :: Monad m => (s -> m ()) -> (s -> m ()) -> IsoM' m s s
acting fwd bwd = isoM (\s -> fwd s $> s) (\s -> bwd s $> s)

from :: Monad m => IsoM m s t a b -> IsoM m b a t s
from l = isoM (rviewIsoM l) (viewIsoM l)
