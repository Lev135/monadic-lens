module Control.Lens.Monadic.Lens where

import Control.Arrow (Kleisli(..))
import Control.Monad ((<=<))
import Data.Functor.Compose (Compose(..))

type LensM m s t a b =
  forall f. TraversableLike m f =>
    (a -> Compose m f b) -> s -> Compose m f t

type LensM' m s a = LensM m s s a a

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter f s =
  Compose $ setter s `fmapM` (getter s >>= getCompose . f)

-- | More restrictive constructor of monadic lens providing effects of getter
-- and setter in both viewing and setting
lensM2 :: Monad m => (s -> m a) -> (s -> m (b -> t)) -> LensM m s t a b
lensM2 sma smbt amfb s = Compose $ do
    a <- sma s
    fb <- getCompose $ amfb a
    bt <- smbt s
    return $ fmap bt fb

{- | The class of 'Functor's @t@, for which traverse-like function for specific
  applicative functor @f@.

  For trivial @f = Identity@ we should get 'fmap' up to 'coerce'.
  @instance Functor f => TraversableLike f t@ is not provided though, since in
  this case it should be INCOHERENT.

  Since type @f@ is restricted only one law from "Data.Traversable" is preserved:

  [Naturality]
    @t . 'traverseLike' f ≡ 'traverseLike' (t . f)@
    for every applicative transformation @t@
-}
class (Applicative f, Functor t) => TraversableLike f t where
  traverseLike :: (a -> f b) -> t a -> f (t b)

{- | This instance is marked as OVERLAPPABLE to escape type errors, when @t@
  is actually not 'Traversable'. However, for real 'Traversable's this instance
  is not expected to be overwritten.
-}
instance {-# OVERLAPPABLE #-} (Applicative f, Traversable t) => TraversableLike f t where
  traverseLike = traverse

{- | @Kleisli m a@ is not a 'Traversable' (and even not 'Foldable').
  However we can define 'traverseLike' function for it's monad @m@.
-}
instance (Monad m) => TraversableLike m (Kleisli m a) where
  traverseLike h = pure . Kleisli . (h <=<) . runKleisli

{- | Convenient derivative from 'traverseLike' function for monads.
  For trivial @m = `Identity`@ is isomorphic to `fmap`, for trivial
  @f = `Identity`@ to monadic bind operator `>>=`.
-}
fmapM :: (Monad m, TraversableLike m f) => forall b t. (b -> m t) -> m (f b) -> m (f t)
fmapM bmt = (traverseLike bmt =<<)
