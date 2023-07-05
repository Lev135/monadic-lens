module FMMF.Getter where

import Control.Applicative
import Control.Monad
import Data.Functor.Contravariant
import Data.Kind

class (Functor m, Functor f) => Distributive' m f where
  distribute' :: m (f a) -> f (m a)
  distribute' = collect' id
  collect' :: (a -> f b) -> m a -> f (m b)
{-
  collect' (fmap h . f) ≡ fmap (fmap h) . collect' f
  collect' (f . g) ≡ collect' f . fmap g
-}
{-
  f :: a -> m b
  g :: b -> m c
  collect' afa :: m c -> f (m d)
  afa :: c -> f d
  collect' afa . g :: b -> f (m d)
  collect' (collect' afa . g) :: m b -> f (m (m d))
  collect' (collect' afa . g) . f :: a -> f (m (m d))
  collect' (phantom . collect' afa . g) . f ≡ collect' afa . (g <=< f)
-}
instance Monad m => Distributive' m (Const (m x)) where
  collect' f = Const . (getConst . f =<<)

instance Functor m => Distributive' m m where
  collect' = fmap

type RightModule :: (Type -> Type) -> (Type -> Type) -> Constraint
class RightModule m f where
  joinR :: f (m a) -> f a

instance Monad m => RightModule m m where
  joinR = join

instance {-# INCOHERENT #-} RightModule m (Const x) where
  joinR = phantom

type Getter s a =
  forall f. (Contravariant f, Functor f) =>
    (a -> f a) -> s -> f s

type GetterM m s a =
  forall f. (Contravariant f, Functor f, Distributive' m f, RightModule m f) =>
    (a -> f a) -> s -> f s

to :: (s -> a) -> Getter s a
to h afa = phantom . afa . h

view :: Getter s a -> s -> a
view l = getConst . l Const

toM :: Functor m => (s -> m a) -> GetterM m s a
toM h afa = phantom . collect' afa . h

viewM :: Monad m => GetterM m s a -> s -> m a
viewM l = getConst . l (Const . pure)

{-
(s -> m a) ≅ GetterM m s a:

  toM   :: (s -> m a) -> GetterM m s a
  viewM :: GetterM m s a -> (s -> m a)

laws:
  viewM . toM ≡ id
  toM . viewM ≡ asGetterM
  toM (f >=> g) ≡ toM f . toM g
  viewM (lf . lg) ≡ viewM lf >=> viewM lg
-}
