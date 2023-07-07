{- | A 'GetterM' is an effectful variant of the 'Control.Lens.Getter.Getter'
  from the lens library.
-}
module Control.Lens.Monadic.Getter (
  -- * Formation
  GetterM,
  -- * Introduction
  toM,
  -- * Elimination
  viewM,
  -- * Computation
  -- | @'GetterM` m s a ≅ s -> m a:
  -- 'toM' . 'viewM' ≡ 'id'
  -- 'viewM' . 'toM' ≡ 'id'
  -- 'toM' f . 'toM' g ≡ 'toM (g . f)'
  -- 'viewM' lg . 'viewM' lf ≡ 'viewM' (lf . lg)@

  -- * Well-formedness
  -- | No laws are provided to getters

  -- * Concrete representation
  GettingM
) where

import Control.Applicative (Const(..))
import Control.Lens.Monadic.Internal
import Control.Monad.Reader (MonadReader(ask))
import Data.Functor ((<&>))
import Data.Functor.Contravariant (Contravariant(..), phantom)

-- | Type synonym for an effectful getter
type GetterM m s a =
  forall f. (Contravariant f, Functor f, Module m f) =>
    (a -> f a) -> s -> f s

-- | Concrete representation of 'GetterM' getting @r@ from the structure @s@.
--
-- If you see a @'GettingM' m a s a@ as an argument of a function it expects
-- @GetterM m s a@.
type GettingM m r s a =
  (a -> Const (m r) a) -> s -> Const (m r) s

-- | Construct an effectful getter from an accessor Kleisli arrow @k@.
--
-- All effects, produced by @k@ are forward.
toM :: Monad m => (s -> m a) -> GetterM m s a
toM h afa = phantom . lbind afa . h

-- | View the value of the target.
--
-- Only forward effects are applied.
--
-- @
-- viewM :: Monad m => 'GetterM' m s a -> s -> m a
-- viewM :: (MonadReader s m', Monad m) => GetterM' m s a -> m' (m a)
-- @
viewM :: (MonadReader s m', Applicative m) => GettingM m a s a -> m' (m a)
viewM l = ask <&> getConst . l (Const . pure)
