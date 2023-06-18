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

{- | Type synonym for effectful lens, providing only forward effects.

  Every 'LensMFwd' is a 'LensM'.
  Since its definition doesn't depend on the 'TraversableLike' class, you can
  provide 'LensMFwd' without depending on this package.

  /NB!/ This type signature doesn't prevent you of specifying something like
  backward effects. For example, you can write the following constructor:

  @
    bad :: Monad m => (s -> m a) -> (s -> m (b -> t)) -> LensMFwd m s t a b
    bad getter setter f s = Compose $ do
        a <- getter s
        fb <- getCompose $ f a
        bt <- setter s
        return $ fmap bt fb
  @

  However, it breaks our division on forward and backward effects: the effects
  from @setter@ would apply in backward direction both while viewing and setting.
-}
type LensMFwd m s t a b =
  forall f. Functor f =>
    (a -> Compose m f b) -> s -> Compose m f t

{- | Type synonym for type-preserving 'LensMFwd'
-}
type LensMFwd' m s a = LensMFwd m s s a a

-- | More restrictive constructor of monadic lens providing effects of getter
-- and setter in both viewing and setting
lensMFwd :: Monad m => (s -> m a) -> (s -> b -> t) -> LensMFwd m s t a b
lensMFwd getter setter f s =
  Compose $ (setter s <$>) <$> (getter s >>= getCompose . f)


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
