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
  {- |
    @l ≡ 'toM' k => viewM l ≡ k@
  -}
  -- * Well-formedness
  {- |
    No laws are provided to getters
  -}
  -- * Concrete representation
  GettingM
) where

import Control.Applicative (Const(..))
import Control.Monad.Reader (MonadReader(ask))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant(..), phantom)

{- | Type synonym for an effectful setter
-}
type GetterM m s a =
  forall f. (Contravariant f, Functor f) =>
    (a -> Compose m f a) -> s -> Compose m f s

{- | Concrete representation of 'GetterM' getting @r@ from the structure @s@.

  If you see a @'GettingM' m a s a@ as an argument of a function it expects
  @GetterM m s a@.
-}
type GettingM m r s a =
  (a -> Compose m (Const r) a) -> s -> Compose m (Const r) s

{- | Construct an effectful getter from an accessor Kleisli arrow @k@.

  All effects, produced by @k@ are forward.
-}
toM :: Monad m => (s -> m a) -> GetterM m s a
toM g f s = Compose $ g s >>= fmap phantom . getCompose . f

{- | View the value of the target.

  Only forward effects are applied.

  @
    viewM :: Monad m => 'GetterM' m s a -> s -> m a
    viewM :: (MonadReader s m', Monad m) => GetterM' m s a -> m' (m a)
  @
-}
viewM :: (MonadReader s m', Applicative m) => GettingM m a s a -> m' (m a)
viewM l = ask <&> \s -> getConst <$> getCompose (l (Compose . pure . Const) s)
