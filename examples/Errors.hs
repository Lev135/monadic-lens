{-# LANGUAGE OverloadedLabels #-}

module Errors where

import Control.Lens
import Control.Lens.Monadic
import Control.Monad.Except (MonadError(..))
import Data.Generics.Labels ()
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)

type Record = Map String Value

data Value
  = I Int
  | R Record
  deriving (Generic, Show)

record :: [(String, Value)] -> Value
record = R . M.fromList

data Err
  = NotLit Value
  | NotRecord Value
  | NoField String [String]
  deriving (Show)

mTravFirst :: MonadError e m => Traversal' s a -> (s -> e) -> LensM' m s a
mTravFirst l e = lensM getter setter
  where
    getter s = case firstOf l s of
      Nothing -> throwError $ e s
      Just a  -> pure a
    setter s a = pure $ set l a s

mLit :: LensM' (Either Err) Value Int
mLit = mTravFirst #_I NotLit

mRecord :: LensM' (Either Err) Value Record
mRecord = mTravFirst #_R NotRecord

mField :: String -> LensM' (Either Err) Record Value
mField k = mTravFirst (ix k) (NoField k . M.keys)

{-
>>> tree = record [("a", I 1), ("b", record [("x", record []), ("y", record [("v", I 42)])])]

We can access to fields like with simple lens:

>>> viewM (mRecord . mField "b" . mRecord . mField "y") tree
Right (R (fromList [("v",I 42)]))

But if something go wrong we'll get a useful error message:

>>> viewM (mRecord . mField "c" . mRecord . mField "y") tree
Left (NoField "c" ["a","b"])
>>> viewM (mRecord . mField "b" . mRecord . mField "z") tree
Left (NoField "z" ["x","y"])

We can use the same lenses for updates:

>>> setM (mRecord . mField "b" . mRecord . mField "y") (pure $ I 42) tree
Right (R (fromList [("a",I 1),("b",R (fromList [("x",R (fromList [])),("y",I 42)]))]))

And good errors preserves:

>>> setM (mRecord . mField "c" . mRecord . mField "y") (pure $ I 42) tree
Left (NoField "c" ["a","b"])
>>> setM (mRecord . mField "b" . mRecord . mField "z") (pure $ I 42) tree
Left (NoField "z" ["x","y"])

But what if we want to insert new element z?
In this case we should use simple non-monadic lens @at "z"@. It can be used
without any special lifting, since @LensM m@ is just a special case of @Lens@

>>> setM (mRecord . mField "b" . mRecord . at "z") (pure $ Just $ I 42) tree
Right (R (fromList [("a",I 1),("b",R (fromList [("x",R (fromList [])),("y",R (fromList [("v",I 42)])),("z",I 42)]))]))

-}
