module Control.Lens.Monadic.Traversal where

import Control.Lens.Monadic.Internal

type TraversalM m s t a b
  = forall f. (Applicative (f m), Join f) =>
    (a -> f m b) -> (s -> f m t)

type TraversalM' m s a = TraversalM m s s a a

-- traversalM :: ((a -> m (f b)) -> s -> m (f t)) -> LensLike f s t a b
-- traversalM l afb = Compose . l (getCompose . afb)

-- traverseOfM :: LensLike f s t a b -> (a -> m (f b)) -> s -> m (f t)
-- traverseOfM l afb = getCompose . l (Compose . afb)
