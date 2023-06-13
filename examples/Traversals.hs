{-# OPTIONS_GHC -Wno-unused-imports #-}
module Traversals where

import Control.Lens
import Control.Lens.Monadic.Iso
import Control.Lens.Monadic.Setter
import Control.Lens.Monadic.Traversal
import Control.Lens.Monadic.Type
import Control.Monad.Writer
import Data.Functor (($>))
import Prelude hiding (log)

type Log = [String]

log :: Show s => IsoM' (Writer Log) s s
log = acting (\s -> tell ["forward " ++ show s])
             (\s -> tell ["backward " ++ show s])

run :: Show a => Writer Log a -> IO ()
run wa = do
  let (a, w) = runWriter wa
  print a
  putStrLn $ unlines w

justFirst :: TraversalM' (Writer Log) (Maybe (Int, Bool)) Int
justFirst = log . _Just . log . _1 . log

justFirsts :: TraversalM' (Writer Log) [Maybe (Int, Bool)] Int
justFirsts = traversed . justFirst

test :: (Show b) => LensLikeM (Writer Log) [] s t b b -> s -> Writer Log [t]
test l = traverseOfM l $ \i -> tell ["found " ++ show i] $> [i]

{-
ghci> run $ test justFirst $ Just (42, False)
[Just (42,False)]
forward Just (42,False)
forward (42,False)
forward 42
found 42
backward 42
backward (42,False)
backward Just (42,False)

ghci> run $ test justFirst $ Nothing
[Nothing]
forward Nothing
backward Nothing

ghci> run $ test justFirsts $ [Just (42, False), Nothing, Just (13, True)]
[[Just (42,False),Nothing,Just (13,True)]]
forward Just (42,False)
forward (42,False)
forward 42
found 42
backward 42
backward (42,False)
backward Just (42,False)
forward Nothing
backward Nothing
forward Just (13,True)
forward (13,True)
forward 13
found 13
backward 13
backward (13,True)
backward Just (13,True)

ghci> run $ setM justFirst (pure 0) $ Just (42, False)
Just (0,False)
forward Just (42,False)
forward (42,False)
forward 42
backward 0
backward (0,False)
backward Just (0,False)

ghci> run $ setM justFirsts (pure 100) $ [Just (42, False), Nothing, Just (13, True)]
[Just (100,False),Nothing,Just (100,True)]
forward Just (42,False)
forward (42,False)
forward 42
backward 100
backward (100,False)
backward Just (100,False)
forward Nothing
backward Nothing
forward Just (13,True)
forward (13,True)
forward 13
backward 100
backward (100,True)
backward Just (100,True)

-}
