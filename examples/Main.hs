{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures  #-}
module Main where
import Control.Lens
import Control.Lens.Monadic
import Control.Monad.State

main :: IO ()
main = pure ()


data Law = Law

(===) :: a -> a -> Law
_ === _ = Law
infix 0 ===

(&&&) :: Law -> Law -> Law
_ &&& _ = Law
infix 0 &&&


(~=~) :: Applicative m => m a -> m a -> Law
ma ~=~ mb = (ma <* mb === mb) &&& (mb <* ma === ma)

(~~) :: (Eq (m a), Applicative m) => m a -> m a -> Bool
ma ~~ mb = ((mb *> ma) == mb) || ((ma *> mb) == ma)

putPut :: Setter s s a b -> b -> b -> s -> Law
putPut l v v' s = set l v' (set l v s) === set l v' s

putPutM :: Monad m => SetterM m s s a b -> m b -> m b -> s -> Law
putPutM l v v' s = setM l v' =<< setM l v s === setM l v' s <* setM l v s

overId :: Setter s s a a -> s -> Law
overId l s = over l id s === s

overIdM :: Monad m => SetterM m s s a a -> s -> Law
overIdM l s = overM l pure s === s <$ overM l pure s

overCompose :: Setter s s a a  -> (a -> a) -> (a -> a) -> s -> Law
overCompose l f g s = (over l f . over l g) s === over l (f . g) s

overComposeM :: Monad m => SetterM m s s a a -> (a -> m a) -> (a -> m a) -> s -> Law
overComposeM l f g s = (overM l f <=< overM l g) s === overM l (f <=< g) s

-- view l (set l v s) ≡ v
getPutM :: Monad m => LensM m s s a a -> m a -> s -> Law
getPutM l v s = v *> (viewM l =<< setM l v s) === v <* (viewM l =<< setM l v s)

-- set l (view l s) s ≡ s
putGetM :: Monad m => LensM m s s a a -> s -> Law
putGetM l s = setM l (viewM l s) s === s <$ setM l (viewM l s) s

{-
  Setter:
    PutPut:
      setM l v' =<< setM l v s === setM l v' s <* setM l v s
    OverId:
      overM l pure s === s <$ overM l pure s
    OverCompose:
      not applicable since `overM l f <=< overM l g` and `overM l (f <=< g)`
      has essentially different effects sequence
  Getter:
    No laws
  Lens:
    GetPut:
      v *> (viewM l =<< setM l v s) === v <* (viewM l =<< setM l v s)
    PutGet:
      setM l (viewM l s) s === s <$ setM l (viewM l s) s

-}
