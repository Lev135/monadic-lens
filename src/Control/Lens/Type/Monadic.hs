module Control.Lens.Type.Monadic where

import Data.Functor.Compose (Compose)

type OpticalM m p q f s t a b = p a (Compose m f b) -> q s (Compose m f t)

type OpticalM' m p q f s a = p a (Compose m f a) -> q s (Compose m f s)
