module Control.Lens.Monadic.Prism where

import Control.Arrow (Kleisli(..))
import Control.Lens (Choice(..), Profunctor(..))
import Control.Lens.Monadic.Internal
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor(..))
import Data.Tagged (Tagged(..), retag)

type PrismM m s t a b =
  forall p f. (Choice p, Applicative f, MPF m p f, RightModule m f) =>
    p a (f b) -> p s (f t)

type PrismM' m s a = PrismM m s s a a

prismM :: forall m s t a b. Monad m =>
    (b -> m t) -> (s -> m (Either t a)) -> PrismM m s t a b
prismM bmt smeta = mpf smeta
                 . rmap (either pure (rbind bmt))
                 . right'

buildM :: Monad m => PrismM m s t a b -> b -> m t
buildM l = runKleisli (unTagged $ l $ Tagged $ Kleisli pure)

matchingM :: Monad m => PrismM m s t a b -> s -> m (Either t a)
matchingM l = fmap (either (fmap Left . getTrivialModule) (pure . Right) =<<)
            . unForgetEM $ l (ForgetEM (pure . Right))

class (Monad m, Profunctor p, Functor f) => MPF m p f where
  mpf :: (a -> m b) -> p b (f c) -> p a (f c)

instance (Monad m, LeftModule m f) => MPF m (->) f where
  mpf amb bfc = lbind bfc . amb

instance (Monad m, Functor f) => MPF m Tagged f where
  mpf _ = retag

newtype ForgetEM m a s t
  = ForgetEM { unForgetEM :: s -> m (Either t a) }

instance Functor m => Profunctor (ForgetEM m a) where
  dimap l r = ForgetEM . dimap l (fmap $ first r) . unForgetEM

instance Applicative m => Choice (ForgetEM m a) where
  right' = ForgetEM
         . (\f -> either (pure . Left . Left) (fmap (first Right) . f))
         . unForgetEM

instance (Monad m, Functor f) => MPF m (ForgetEM m x) f where
  mpf amb = ForgetEM . (amb >=>) . unForgetEM
