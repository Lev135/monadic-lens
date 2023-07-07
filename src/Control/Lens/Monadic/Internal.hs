{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
module Control.Lens.Monadic.Internal where

import Control.Applicative
import Control.Arrow
import Control.Monad
-- import Data.Functor.Compose
import Data.Functor.Contravariant


class (Monad m, Functor f) => LeftModule m f | f -> m where
  ljoin :: m (f a) -> f a

lbind :: LeftModule m f => (a -> f b) -> m a -> f b
lbind k = ljoin . fmap k

class (Monad m, Functor f) => RightModule m f where
  rjoin :: f (m a) -> f a

rbind :: RightModule m f => (a -> m b) -> f a -> f b
rbind k = rjoin . fmap k

type Module m f = (LeftModule m f, RightModule m f)

-- Const functor

instance Monad m => LeftModule m (Const (m x)) where
  ljoin = Const . join . fmap getConst

instance (Monad m) => RightModule m (Const x) where
  rjoin = phantom

-- Kleisli arrow

instance Monad m => RightModule m (Kleisli m a) where
  rjoin = Kleisli . fmap join . runKleisli

-- Compose functor

-- instance (LeftModule m n, Functor f) => LeftModule m (Compose n f) where
--   ljoin = Compose . ljoin . fmap getCompose

-- instance (RightModule m n, Functor f) => RightModule m (Compose f n) where
--   rjoin = Compose . fmap rjoin . getCompose

-- Pure monad

newtype TrivialModule m a
  = TrivialModule { getTrivialModule :: m a }
  deriving (Applicative, Functor, Monad)

instance Monad m => LeftModule m (TrivialModule m) where
  ljoin = TrivialModule . join . fmap getTrivialModule

instance Monad m => RightModule m (TrivialModule m) where
  rjoin = TrivialModule . join . getTrivialModule
