{-# LANGUAGE LambdaCase #-}
module Control.Lens.Monadic.Prism where

import Control.Arrow (Kleisli(..))
import Control.Lens (Choice(..), Identity(..), Profunctor(..))
import Control.Lens.Monadic.Lens (FunctorM(..))
import Control.Monad ((<=<), (>=>))
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Compose (Compose(..))
import Data.Tagged (Tagged(..), retag)

type PrismM m s t a b =
  forall p f. (Choice p, Applicative f, ProfunctorM m p, FunctorM m f) =>
    p a (Compose m f b) -> p s (Compose m f t)

type PrismM' m s a = PrismM m s s a a

prismM :: forall m s t a b. Monad m =>
    (b -> m t) -> (s -> m (Either t a)) -> PrismM m s t a b
prismM bt seta = rmap Compose
               . fish seta
               . rmap (either (pure . pure) (fmapM bt . getCompose))
               . right'

buildM :: Monad m => PrismM m s t a b -> b -> m t
buildM l b = unpack (l (pack $ pure pure)) >>= ($ b)
  where
    pack = Tagged . Compose . fmap Kleisli
    unpack = fmap runKleisli . getCompose . unTagged

matchingM :: Monad m => PrismM m s t a b -> s -> m (Either t a)
matchingM l = case l $ SemiMarketM (pure . Right) of
  SemiMarketM seta ->
    either (fmap (Left . runIdentity) . getCompose) (pure . Right) <=< seta

class (Monad m, Profunctor p) => ProfunctorM m p where
  fish :: forall a b c. (a -> m b) -> p b (m c) -> p a (m c)

instance Monad m => ProfunctorM m (->) where
  fish = (>=>)

instance Monad m => ProfunctorM m Tagged where
  fish _ = retag


newtype SemiMarketM m a s t
  = SemiMarketM (s -> m (Either t a))

instance Functor m => Functor (SemiMarketM m a s) where
  fmap f (SemiMarketM seta) = SemiMarketM $ fmap (first f) . seta

instance Functor m => Profunctor (SemiMarketM m a) where
  dimap l r (SemiMarketM seta) = SemiMarketM $ fmap (first r) . seta . l

instance Monad m => Choice (SemiMarketM m a) where
  left' :: SemiMarketM m a s t -> SemiMarketM m a (Either s x) (Either t x)
  left' (SemiMarketM seta) = SemiMarketM $ \case
    Left  s -> seta s >>= \case
      Left  t -> pure $ Left $ Left t
      Right a -> pure $ Right a
    Right x -> pure $ Left (Right x)

instance Monad m => ProfunctorM m (SemiMarketM m a) where
  fish :: (u -> m s) -> SemiMarketM m a s (m t) -> SemiMarketM m a u (m t)
  fish k (SemiMarketM seta) = SemiMarketM $ k >=> seta
