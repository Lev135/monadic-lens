{-# LANGUAGE LambdaCase #-}
module Parts.Matcher.NonMonadic where

import Control.Lens (Choice(..), Profunctor(..))
import Control.Monad.Identity (Identity(..))
import Data.Bifunctor (Bifunctor(..))

type AMatcher s t a =
    SemiMarket a a (Identity a) -> SemiMarket a s (Identity t)

aMatcher :: (s -> Either t a) -> AMatcher s t a
aMatcher seta _ = SemiMarket (first Identity . seta)

match :: AMatcher s t a -> s -> Either t a
match l = case l $ SemiMarket Right of
  SemiMarket seta -> first runIdentity . seta

newtype SemiMarket a s t
  = SemiMarket (s -> Either t a)

instance Functor (SemiMarket a s) where
  fmap f (SemiMarket seta) = SemiMarket $ first f . seta

instance Profunctor (SemiMarket a) where
  dimap l r (SemiMarket seta) = SemiMarket $ first r . seta . l

instance Choice (SemiMarket a) where
  left' :: SemiMarket a s t -> SemiMarket a (Either s x) (Either t x)
  left' (SemiMarket seta) = SemiMarket $ \case
    Left  s -> case seta s of
      Left  t -> Left $ Left t
      Right a -> Right a
    Right x -> Left (Right x)
