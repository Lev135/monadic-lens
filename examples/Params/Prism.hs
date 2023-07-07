{-# LANGUAGE TypeFamilies #-}
module Params.Prism where

import Control.Lens (Choice(..), Profunctor(..))
import Control.Monad
import Control.Monad.Identity (IdentityT(runIdentityT))
import Data.Bifunctor
import Data.Tagged
import Params.Trans

type PrismM m s t a b =
  forall k (p :: k) f. (Choice (ParP p m), Applicative (f m), Fish p f, JoinInner f) =>
    ParP p m a (f m b) -> ParP p m s (f m t)

prismM :: forall m s t a b. Monad m =>
    (b -> m t) -> (s -> m (Either t a)) -> PrismM m s t a b
prismM bmt smeta = fish smeta
                 . rmap (either pure (bindIn bmt))
                 . right'

buildM :: Monad m => PrismM m s t a b -> b -> m t
buildM l = runKleisliT (unTagged $ l $ Tagged $ KleisliT pure)

matchingM :: Monad m => PrismM m s t a b -> s -> m (Either t a)
matchingM l = fmap (either (fmap Left . runIdentityT) (pure . Right) =<<)
            . unForgetEM $ l (ForgetEM (pure . Right))

newtype ForgetEM a m s t
  = ForgetEM { unForgetEM :: s -> m (Either t a) }

instance Functor m => Profunctor (ForgetEM a m) where
  dimap l r = ForgetEM . dimap l (fmap $ first r) . unForgetEM

instance Applicative m => Choice (ForgetEM a m) where
  right' = ForgetEM
         . (\f -> either (pure . Left . Left) (fmap (first Right) . f))
         . unForgetEM

instance Fish (ForgetEM x) f where
  type ParP (ForgetEM x) m = ForgetEM x m
  fish amb = ForgetEM . (amb >=>) . unForgetEM
