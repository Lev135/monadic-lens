module Control.Lens.Monadic.Fold where

import Control.Applicative (Const(..))
import Control.Lens.Internal.Fold (Leftmost(..), getLeftmost)
import Control.Lens.Monadic.Getter (GettingM, viewM)
import Control.Lens.Monadic.Internal
import Control.Monad ((<=<), (>=>))
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (Contravariant, phantom)
import Data.Monoid (First(..))

type FoldM m s a =
  forall f. (Contravariant f, Applicative f, Module m f) =>
    (a -> f a) -> (s -> f s)

foldingM :: (Monad m, Foldable t) => (s -> m (t a)) -> FoldM m s a
foldingM smta afa = lbind (phantom . traverse_ afa) . smta

foldMapOfM :: Functor m => GettingM m r s a -> (a -> m r) -> s -> m r
foldMapOfM l amr = getConst . l (Const . amr)

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
