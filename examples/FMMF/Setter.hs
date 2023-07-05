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

type SetterM m s t a b =
  forall f. m ~ f => (a -> f b) -> s -> f t

settingM :: Applicative m => ((a -> m b) -> s -> m t) -> SetterM m s t a b
settingM h afb = h afb

overM :: SetterM m s t a b -> (a -> m b) -> s -> m t
overM l amb s = l amb s
