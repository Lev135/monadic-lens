module Control.Lens.Lens.Monadic where

import Control.Applicative (Const(..))
import Control.Monad.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

class Functor f => MfaAms f where
  mfaAms :: forall m a s. Monad m => m (f a) -> (a -> m s) -> m (f s)

instance MfaAms (Const a) where
  mfaAms mfa _ = Const . getConst <$> mfa

instance MfaAms Identity where
  mfaAms mfa ams = Identity <$> (mfa >>= (ams . runIdentity))

type LensM m s t a b =
  forall f. MfaAms f =>
    (a -> Compose m f b) -> s -> Compose m f t

type LensM' m s a = LensM m s s a a

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter f s =
  Compose $ (getter s >>= getCompose . f) `mfaAms` setter s
