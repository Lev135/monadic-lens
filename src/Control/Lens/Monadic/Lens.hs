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
) where

import Control.Lens.Monadic.Internal

-- | Type synonym for type-modifying effectful lens
type LensM m s t a b =
  forall f. (Functor f, Module m f) =>
    (a -> f b) -> s -> f t

{-| Type synonym for type-preserving effectful lens
-}
type LensM' m s a = LensM m s s a a

lensM :: Monad m => (s -> m a) -> (s -> b -> m t) -> LensM m s t a b
lensM getter setter afb s = rbind (setter s) $ lbind afb (getter s)
