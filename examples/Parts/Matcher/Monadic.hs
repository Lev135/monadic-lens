{-# LANGUAGE LambdaCase #-}
module Parts.Matcher.Monadic where

import Control.Lens (Choice(..), Profunctor(..))
import Control.Monad.Identity (Identity(..), (<=<))
import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Compose
{-
  type ABuilderM m s t a = p a (Compose m f a) -> p s (Compose m f t)
    where
      p = SemiMarketM m a
      f = Identity
-}
type AMatcherM m s t a =
  SemiMarketM m a a (Compose m Identity a) -> SemiMarketM m a s (Compose m Identity t)

aMatcherM :: Applicative m => (s -> m (Either t a)) -> AMatcherM m s t a
aMatcherM seta _ = SemiMarketM (fmap (first (Compose . pure . Identity)) . seta)

matchM :: Monad m => AMatcherM m s t a -> s -> m (Either t a)
matchM l = case l $ SemiMarketM (pure . Right) of
  SemiMarketM seta ->
    either (fmap (Left . runIdentity) . getCompose) (pure . Right) <=< seta

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
