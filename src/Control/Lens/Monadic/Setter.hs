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

-- * Concrete representation
  ASetterM, ASetterM'
) where

import Control.Lens.Monadic.Internal
import Control.Monad.Identity
import Data.Coerce

-- | Type synonym for type-modifying effectful setter
type SetterM m s t a b =
  forall f. (SettableT f, Functor (f m), Join f) => (a -> f m b) -> s -> f m t

-- | Type synonym for type-preserving effectful setter
type SetterM' m s a = SetterM m s s a a

type ASetterM m s t a b = (a -> IdentityT m b) -> s -> IdentityT m t

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
settingM ambsmt amb = fromApp . ambsmt (toApp . amb)

{- | Modify a target by an effectful function.

Effects sequence of @overM l h@

@forward l >>= h >>= backward l@
-}
overM :: Functor m => ASetterM m s t a b -> (a -> m b) -> s -> m t
overM = coerce

{- | Set a value to the target.

prop> setM l b = overM l (const b)

Effects sequence of @setM l v@:

@forward l >> v >>= backward l@
-}
setM :: Functor m => ASetterM m s t a b -> m b -> s -> m t
setM l = overM l . const

class SettableT f where
  fromApp :: m a -> f m a
  toApp   :: f m a -> m a

instance SettableT IdentityT where
  fromApp = coerce
  toApp = coerce
