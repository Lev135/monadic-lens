module Control.Lens.Lens.Monadic where

import Control.Applicative (Const(..))
import Control.Monad.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

{-
  law> fmap h = runIdentity . gfmap (Identity . h) . Identity
-}
class Functor f => FunctorM f where
  fmapM :: forall m b t. Monad m => (b -> m t) -> m (f b) -> m (f t)

instance FunctorM (Const a) where
  fmapM _ c = Const . getConst <$> c

instance FunctorM Identity where
  fmapM bmt mb = Identity <$> (mb >>= (bmt . runIdentity))

type LensM m s t a b =
  forall f. FunctorM f =>
    (a -> Compose m f b) -> s -> Compose m f t

type LensM' m s a = LensM m s s a a

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter f s =
  Compose $ setter s `fmapM` (getter s >>= getCompose . f)
