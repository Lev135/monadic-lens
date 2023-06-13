module Control.Lens.Fold.Monadic where

import Control.Applicative (Const(..))
import Control.Lens.Getter.Monadic (GettingM, viewM)
import Control.Lens.Internal.Fold (Leftmost(..), getLeftmost)
import Control.Monad ((<=<), (>=>))
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant, phantom)
import Data.Monoid (First(..))

type FoldM m s a =
  forall f. (Contravariant f, Applicative f) =>
    (a -> Compose m f a) -> (s -> Compose m f s)

foldingM :: (Monad m, Foldable t) => (s -> m (t a)) -> FoldM m s a
foldingM g f s = Compose $ g s >>= fmap phantom . getCompose . traverse_ f

foldMapOfM :: Functor m => GettingM m r s a -> (a -> m r) -> s -> m r
foldMapOfM l ar = fmap getConst . getCompose . l (Compose . fmap Const . ar)

foldOfM :: Applicative m => GettingM m a s a -> s -> m a
foldOfM = viewM

foldrOfM :: Monad m =>
  GettingM m (KleisliEndo m r) s a -> (a -> r -> m r) -> m r -> s -> m r
foldrOfM l f z =
  (\x -> z >>= appKleisliEndo x) <=< foldMapOfM l (pure . KleisliEndo . f)

toListOfM :: Monad m => GettingM m (KleisliEndo m [a]) s a -> s -> m [a]
toListOfM l = foldrOfM l (fmap pure . (:)) (pure [])

firstOfM :: Monad m => GettingM m (Leftmost a) s a -> s -> m (Maybe a)
firstOfM l = fmap getLeftmost . foldMapOfM l (pure . LLeaf)

previewM :: (Monad m, MonadReader s m') => GettingM m (First a) s a -> m' (m (Maybe a))
previewM l = asks (fmap getFirst . foldMapOfM l (pure . First . Just))

newtype KleisliEndo m a
  = KleisliEndo { appKleisliEndo :: a -> m a }

instance Monad m => Semigroup (KleisliEndo m a) where
  k <> k' = KleisliEndo $ appKleisliEndo k >=> appKleisliEndo k'

instance Monad m => Monoid (KleisliEndo m a) where
  mempty = KleisliEndo pure
