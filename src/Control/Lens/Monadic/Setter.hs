{-  | A 'SetterM' is an effectful variant of the 'Control.Lens.Setter.Setter'
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
-- |
-- @l ≡ 'settingM' f => 'overM' l ≡ f@

-- * Well-formedness
-- |
-- [PutPut]
--
-- @'setM' l v' '=<<' 'setM' l v s ≈ 'setM' l v' s@
--
-- [Functoriality]
--
-- @'overM' l 'pure' ≈ 'pure'
-- 'overM' l f '<=<' 'overM' l g ≈ 'overM' l (f '<=<' g)@
--
-- See more about laws in "Control.Lens.Monadic#g:laws"
) where
import qualified Control.Lens as L

-- | Type synonym for type-modifying effectful setter
type SetterM m s t a b =
  (a -> m b) -> s -> m t

-- | Type synonym for type-preserving effectful setter
type SetterM' m s a = SetterM m s s a a

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
settingM = id

{- | Modify a target by an effectful function.

Effects sequence of @overM l h@

@forward l >>= h >>= backward l@
-}
overM :: Functor m => SetterM m s t a b -> (a -> m b) -> s -> m t
overM = id

{- | Set a value to the target.

Effects sequence of @setM l v@:

@forward l >> v >>= backward l@
-}
setM :: Functor m => SetterM m s t a b -> m b -> s -> m t
setM l b = l (const b)
