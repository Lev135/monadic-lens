module Control.Lens.Monadic.Getter where

import Control.Applicative (Const(..))
import Control.Monad.Reader (MonadReader(ask))
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant(..), phantom)

type GetterM m s a =
  forall f. (Contravariant f, Functor f) =>
    (a -> Compose m f a) -> s -> Compose m f s

type GettingM m r s a =
  (a -> Compose m (Const r) a) -> s -> Compose m (Const r) s

toM :: Monad m => (s -> m a) -> GetterM m s a
toM g f s = Compose $ g s >>= fmap phantom . getCompose . f

viewM :: (MonadReader s m', Applicative m) => GettingM m a s a -> m' (m a)
viewM l = ask <&> \s -> getConst <$> getCompose (l (Compose . pure . Const) s)
