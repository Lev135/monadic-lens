{-  | A 'SetterM' is effectful variant of the 'Control.Lens.Setter.Setter'
  from the lens library.
-}
module Control.Lens.Monadic.Setter (
  -- * Formation
  SetterM, SetterM',
  -- * Introduction
  settingM,
  -- * Elimination
  overM, setM,
  -- * Computation
  {- |
    @l ≡ 'settingM' f => 'overM' l ≡ f@
  -}
  -- * Well-formedness
  {- |
    * __PutPut__: setting twice is the same as setting once up to effects:

        @
          'setM' l v' '=<<' 'setM' l v s ≈ 'setM' l v' s
        @

    * __Functoriality__: setters must preserve identity and composition up to effects:

        @
          'overM' l 'pure' ≈ 'pure'
          'overM' l f '<=<' 'overM' l g ≈ 'overM' l (f '<=<' g)
        @

    See more about laws in "Control.Lens.Monadic#laws"
  -}
  -- * Concrete representation
  ASetterM, ASetterM'
) where

import Control.Lens.Internal.Setter (Settable(untainted))
import Control.Monad.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

{- | Type synonym for type-modifying effectful setter
-}
type SetterM m s t a b =
  forall f. Settable f =>
    (a -> Compose m f b) -> s -> Compose m f t

{- | Type synonym for type-preserving effectful setter
-}
type SetterM' m s a = SetterM m s s a a

{- | Concrete representation of 'SetterM'.

  When you see this type as an argument to the function, you should provide
  'SetterM' to it.
-}
type ASetterM m s t a b =
  (a -> Compose m Identity b) -> s -> Compose m Identity t

{- | Type-preserving 'ASetterM'
-}
type ASetterM' m s a = ASetterM m s s a a

{- | Construct an effectful setter from a 'mapM'-like function.

  To produce lawful setter, your supplied function @f@ is required to satisfy:

    prop> f 'pure' ≈ 'pure'
    prop> f k '<=<' f l ≈ f (k '<=<' l)

  Through @f@ you can control both forward and backward effects:

  @
    'settingM' \k -> do
        forwardEffects
        b <- k a
        backwardEffects
  @
-}
settingM :: Functor m => ((a -> m b) -> s -> m t) -> SetterM m s t a b
settingM setter f = Compose . fmap pure . setter (fmap untainted . getCompose . f)

{- | Modify a target by an effectful function.

  Effects sequence of @overM l h@

  @
    forward l >>= h >>= backward l
  @
-}
overM :: Functor m => ASetterM m s t a b -> (a -> m b) -> s -> m t
overM l h = fmap runIdentity . getCompose . l (Compose . fmap Identity . h)

{- | Set a value to the target.

  Effects sequence of @setM l v@

  @
    forward l >> v >>= backward l
  @
-}
setM :: Functor m => ASetterM m s t a b -> m b -> s -> m t
setM l b = overM l (const b)
