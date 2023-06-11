module Control.Lens.Traversal.Monadic where

import Control.Lens.Lens.Monadic (FunctorM)
import Control.Lens.Type.Monadic
import Data.Functor.Compose (Compose(..))

type TraversalM m s t a b
  = forall f. (Applicative f, FunctorM m f) =>
      (a -> Compose m f b) -> (s -> Compose m f t)

type TraversalM' m s a = TraversalM m s s a a

traversalM :: ((a -> m (f b)) -> s -> m (f t)) -> LensLikeM m f s t a b
traversalM l afb = Compose . l (getCompose . afb)

traverseOfM :: LensLikeM m f s t a b -> (a -> m (f b)) -> s -> m (f t)
traverseOfM l afb = getCompose . l (Compose . afb)
