{-# LANGUAGE PartialTypeSignatures #-}
module Laws where

import Control.Lens.Monadic
import Control.Monad


valEq :: (Eq (m Bool), Applicative m, Eq a) => m a -> m a -> Bool
valEq ma mb = me == (True <$ me)
  where me = (==) <$> ma <*> mb

-- Setter laws
actPutM :: (Eq t, Eq (m Bool), Monad m) =>
  ASetterM m s t a b -> s -> m b -> m () -> Bool
actPutM l v s act = (act >> setM l s v) `valEq` setM l s v

putPutM :: (Monad m, Eq s, Eq (m Bool)) =>
  SetterM' m s a -> m a -> m a -> s -> Bool
putPutM l v v' s = (setM l v' =<< setM l v s) `valEq` setM l v' s

idM :: (Monad m, Eq (m s), Eq s, Eq (m Bool)) =>
  SetterM' m s a -> s -> Bool
idM l s = overM l pure s `valEq` pure s
-- seems to be equivalent to (overM l pure s == (s <$ overM l pure s))

composeM :: (Monad m, Eq s, Eq (m Bool)) =>
  SetterM' m s a -> s -> (a -> m a) -> (a -> m a) -> Bool
composeM l s f g = (overM l f <=< overM l g) s `valEq` overM l (f <=< g) s

-- Getter laws
actViewM :: (Eq a, Eq (m Bool), Monad m) =>
  GettingM m a s a -> s -> m () -> Bool
actViewM l s act = (act >> viewM l s) `valEq` viewM l s

-- Lens laws
getPutM :: (Monad m, Eq a, Eq (m Bool)) =>
  LensM m s s a a -> m a -> s -> Bool
getPutM l v s = (viewM l =<< setM l v s) `valEq` v

putGetM :: (Monad m, Eq s, Eq (m Bool)) =>
  LensM m s s a a -> s -> Bool
putGetM l s = setM l (viewM l s) s `valEq` pure s
