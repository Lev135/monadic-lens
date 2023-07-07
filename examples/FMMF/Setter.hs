{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE GADTs #-}
module FMMF.Setter where

import Control.Lens.Internal.Setter (Settable(..))
import Control.Monad.Identity

type Setter s t a b =
  forall f. Settable f => (a -> f b) -> s -> f t

setting :: ((a -> b) -> s -> t) -> Setter s t a b
setting h afb = pure . h (untainted . afb)

over :: Setter s t a b -> (a -> b) -> s -> t
over l ab = runIdentity . l (Identity . ab)

replacer :: (b -> s -> t) -> Setter s t () b
replacer h fb = setting (h . ($ ())) fb

set :: Setter s t () b -> b -> s -> t
set l b = runIdentity . l (Identity . const b)

type SetterM m s t a b =
  forall f. (m ~ f) => (a -> f b) -> s -> f t

settingM :: ((a -> m b) -> s -> m t) -> SetterM m s t a b
settingM h = h

overM :: Monad m => SetterM m s t a b -> (a -> m b) -> s -> m t
overM l = l

setM :: Monad m => SetterM m s t a b -> m b -> s -> m t
setM l = overM l . const
