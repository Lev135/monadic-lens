module Control.Lens.Monadic.Lens where

import Control.Arrow (Kleisli(..))
import Control.Monad ((>=>))
import Data.Functor.Compose (Compose(..))

type LensM m s t a b =
  forall f. FunctorM m f =>
    (a -> Compose m f b) -> s -> Compose m f t

type LensM' m s a = LensM m s s a a

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter f s =
  Compose $ setter s `fmapM` (getter s >>= getCompose . f)

-- | More restrictive constructor of monadic lens providing effects of getter
-- and setter in both viewing and setting
lensM2 :: Monad m => (s -> m a) -> (s -> m (b -> t)) -> LensM m s t a b
lensM2 sma smbt amfb s = Compose $ do
    a <- sma s
    fb <- getCompose $ amfb a
    bt <- smbt s
    return $ fmap bt fb

{-
  For trivial @m = `Identity`@ we get `fmap` up to isomorphism,
  for trivial @f = `Identity`@ monadic bind operator `>>=`.
-}
class (Monad m, Functor f) => FunctorM m f where
  fmapM :: forall b t. (b -> m t) -> m (f b) -> m (f t)

instance {-# OVERLAPPABLE #-} (Monad m, Traversable f) => FunctorM m f where
  fmapM bmt mfb = traverse bmt =<< mfb

instance Monad m => FunctorM m (Kleisli m a) where
  fmapM bmt mk = Kleisli . (>=> bmt) . runKleisli <$> mk
