module Control.Lens.Monadic.Lens (
-- * Formation
  LensM, LensM',
-- * Introduction
  lensM,
-- * Elimination
{- | Since every lens is a getter and setter, specializing eliminators we get

@
'Control.Lens.Monadic.Getter.viewM' :: LensM' m s a -> s -> m a
'Control.Lens.Monadic.Setter.overM' :: LensM m s t a b -> (a -> m b) -> s -> m t
'Control.Lens.Monadic.Setter.setM'  :: LensM m s t a b -> m b -> s -> m t
@

Note that only type-preserving lens can be used for viewing. If you have a
type-modifying one, you should pass it to 'viewM' through a
'Control.Lens.Getter.getting' combinator from "Control.Lens.Getter".
-}
-- * Computation
{- |
@l ≡ 'lensM' getter setter => ('Control.Lens.Monadic.Getter.viewM' l ≡ getter, 'Control.Lens.Monadic.Setter.setM' l ≡ setter)@
-}
-- * Well-formedness
{- | Since any lens is a setter 'LensM' should satisfy
"Control.Lens.Monadic.Setter#g:laws" laws.In addition there are three laws
which provide connection between setter and getter:

[PutPut] todo
[GetPut]
[PutGet]
-}
-- * Class used in the 'LensM' type synonym
  TraversableLike(..), fmapM,
-- * Lens with only forward effects
  LensMFwd, LensMFwd', lensMFwd
) where

import Control.Arrow (Kleisli(..))
import Control.Monad ((<=<))
import Data.Functor.Compose (Compose(..))

{- | Type synonym for type-modifying effectful lens
-}
type LensM m s t a b =
  forall f. TraversableLike m f =>
    (a -> Compose m f b) -> s -> Compose m f t

{-| Type synonym for type-preserving effectful lens
-}
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

{- | Construct a 'LensMFwd' from effectful getter and pure setter functions

Effects from getter are forward.
-}
lensMFwd :: Monad m => (s -> m a) -> (s -> b -> t) -> LensMFwd m s t a b
lensMFwd getter setter f s =
  Compose $ (setter s <$>) <$> (getter s >>= getCompose . f)


{- | The class of 'Functor's @t@, for which traverse-like function exists only
for specific applicative functor @f@.

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
