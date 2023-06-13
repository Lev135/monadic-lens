module Control.Lens.Monadic.Type where

import Data.Functor.Compose (Compose)

type OpticalM m p q f s t a b = p a (Compose m f b) -> q s (Compose m f t)

type OpticalM' m p q f s a = p a (Compose m f a) -> q s (Compose m f s)

type LensLikeM m f s t a b = (a -> Compose m f b) -> (s -> Compose m f t)

type LensLikeM' m f s a = (a -> Compose m f a) -> (s -> Compose m f a)
