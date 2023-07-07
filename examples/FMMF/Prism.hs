{-# LANGUAGE MonoLocalBinds #-}

module FMMF.Prism where

import Control.Arrow (Kleisli(..))
import Control.Lens (Choice(..), Profunctor(..))
import Control.Monad.Identity
import Data.Bifunctor
import Data.Tagged
import FMMF.Getter

{-
type ELensM  m s t a b = exists z. (s -> m (a, z), z -> b -> m t)
type EPrismM m s t a b = exists z. (s -> m (Either a z), Either z b -> m t)

-}

type Prism s t a b =
  forall p f. (Choice p, Applicative f) =>
    p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure $ fmap bt) . right'

matching :: Prism s t a b -> s -> Either t a
matching l = unForgetE (rmap runIdentity $ l $ ForgetE Right)

build :: Prism s t a b -> b -> t
build l = unTagged $ l $ Tagged id

newtype ForgetE a s t
  = ForgetE { unForgetE :: s -> Either t a }

instance Profunctor (ForgetE a) where
  dimap l r = ForgetE . dimap l (first r) . unForgetE

instance Choice (ForgetE a) where
  right' = ForgetE . (\f -> either (Left . Left) (first Right . f)) . unForgetE



class (Monad m, Profunctor p, Functor f) => MPF m p f where
  mpf :: (a -> m b) -> p b (f c) -> p a (f c)

instance Monad m => MPF m (->) m where
  mpf = (>=>)

instance (Monad m, Functor f) => MPF m Tagged f where
  mpf _ = retag

instance (Monad m, Functor f) => MPF m (ForgetEM m x) f where
  mpf amb = ForgetEM . (amb >=>) . unForgetEM

type PrismM m s t a b =
  forall p f. (Choice p, MPF m p f, Applicative f, RightModule m f) =>
    p a (f b) -> p s (f t)

prismM :: (b -> m t) -> (s -> m (Either t a)) -> PrismM m s t a b
prismM bmt smeta = mpf smeta
                 . rmap (either pure (rjoin . fmap bmt))
                 . right'

matchingM :: Monad m => PrismM m s t a b -> s -> m (Either t a)
matchingM l = fmap (either (fmap Left) (pure . Right) =<<)
            . unForgetEM $ l (ForgetEM (pure . Right))

buildM :: Monad m => PrismM m s t a b -> b -> m t
buildM l = runKleisli (unTagged $ l $ Tagged $ Kleisli pure)

newtype ForgetEM m a s t
  = ForgetEM { unForgetEM :: s -> m (Either t a) }

instance Functor m => Profunctor (ForgetEM m a) where
  dimap l r = ForgetEM . dimap l (fmap $ first r) . unForgetEM

instance Applicative m => Choice (ForgetEM m a) where
  right' = ForgetEM
         . (\f -> either (pure . Left . Left) (fmap (first Right) . f))
         . unForgetEM


type PrismMFwd m s t a b =
  forall p f. (Choice p, MPF m p f, Applicative f, LeftModule m f) =>
    p a (f b) -> p s (f t)

prismMFwd :: (b -> t) -> (s -> m (Either t a)) -> PrismMFwd m s t a b
prismMFwd bt smeta = mpf smeta
                   . rmap (either pure (fmap bt))
                   . right'

type PrismMBwd m s t a b =
  forall p f. (Choice p, Applicative f, RightModule m f) =>
    p a (f b) -> p s (f t)

prismMBwd :: (b -> m t) -> (s -> Either t a) -> PrismMBwd m s t a b
prismMBwd bmt seta = dimap seta (either pure (rjoin . fmap bmt)) . right'
