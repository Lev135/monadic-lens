module Control.Lens.Monadic.Type where

import Control.Lens
import Data.Functor.Compose (Compose)
import Data.Functor.Contravariant

type OpticalM m p q f s t a b = p a (Compose m f b) -> q s (Compose m f t)

type OpticalM' m p q f s a = p a (Compose m f a) -> q s (Compose m f s)

type LensLikeM m f s t a b = (a -> Compose m f b) -> (s -> Compose m f t)

type LensLikeM' m f s a = (a -> Compose m f a) -> (s -> Compose m f a)

type LensFM m s t a b = forall f. Functor f => (a -> f (m b)) -> s -> f (m t)

modifyFM :: LensFM m s t a b -> (a -> m b) -> s -> m t
modifyFM l f s = runIdentity $ l (Identity . f) s

viewFM :: Applicative m => LensFM m s t a b -> s -> m a
viewFM l s = getConst $ l (Const . pure) s

-- lensFM :: (s -> m a) -> (s -> b -> m t) -> LensFM m s t a b
-- lensFM getter setter f s = _

class (Monad m, Functor f) => Foo m f where
  foo :: forall a t. m a -> (a -> f (m t)) -> f (m t)

instance Monad m => Foo m (Const (m x)) where
  foo ma amx = Const $ ma >>= getConst . amx

instance Monad m => Foo m Identity where
  foo ma amt = Identity $ ma >>= runIdentity . amt

class Functor f => JoinF f where
  joinF :: forall x m. Monad m => m (f (m x)) -> m (f x)

instance JoinF (Const y) where
  joinF = fmap phantom

instance JoinF Identity where
  joinF = fmap Identity . (runIdentity =<<)

class Functor f => Distributive f where
  distribute :: forall m x. Monad m => f (m x) -> m (f x)

instance Distributive (Const y) where
  distribute = pure . phantom

instance Distributive Identity where
  distribute = fmap Identity . runIdentity

{-
getter s :: m a
setter s :: b -> m t
f :: a -> f (m b)
fmap (>>= setter s) . f :: a -> f (m t)
_ :: f (m t)

f ~ Const (m a)
f :: a -> Const (m a) (m b)
getConst . f :: a -> m a


-}
