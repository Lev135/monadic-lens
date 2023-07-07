{-# LANGUAGE MonoLocalBinds #-}
module FMMF.Iso where
import Control.Lens
import FMMF.Getter
import FMMF.Prism

type IsoM m s t a b =
  forall p f. (MPF m p f, Functor f, RightModule m f) =>
    p a (f b) -> p s (f t)

isoM :: Monad m => (s -> m a) -> (b -> m t) -> IsoM m s t a b
isoM sma bmt = mpf sma . rmap (rjoin . fmap bmt)
