{-# OPTIONS_GHC -Wno-unused-imports #-}

module Log where

import Control.Lens
import Control.Lens.Monadic.Getter
import Control.Lens.Monadic.Iso
import Control.Lens.Monadic.Lens
import Control.Lens.Monadic.Prism
import Control.Lens.Monadic.Setter
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

fstFst :: LensM' (Writer Log) ((Int, String), Bool) Int
fstFst = log . _1 . log . _1 . log

leftJustLeft :: PrismM' (Writer Log) (Either (Maybe (Either Int String)) Bool) Int
leftJustLeft = log . _Left . log . _Just . log . _Left . log

{-
Lens:

ghci> run $ viewM fstFst ((2, "Hello, world"), False)
2
forward ((2,"Hello, world"),False)
forward (2,"Hello, world")
forward 2

ghci> run $ setM fstFst (pure 42) ((2, "Hello, world"), False)
((42,"Hello, world"),False)
forward ((2,"Hello, world"),False)
forward (2,"Hello, world")
forward 2
backward 42
backward (42,"Hello, world")
backward ((42,"Hello, world"),False)

ghci> run $ setM fstFst (tell ["!!!"] $> 42) ((2, "Hello, world"), False)
((42,"Hello, world"),False)
forward ((2,"Hello, world"),False)
forward (2,"Hello, world")
forward 2
!!!
backward 42
backward (42,"Hello, world")
backward ((42,"Hello, world"),False)

Prisms:

ghci> run $ matchM leftJustLeft  (Left (Just (Left 1)))
Right 1
forward Left (Just (Left 1))
forward Just (Left 1)
forward Left 1
forward 1

ghci> run $ matchingM leftJustLeft  (Left (Just (Right "Hello")))
Left (Left (Just (Right "Hello")))
forward Left (Just (Right "Hello"))
forward Just (Right "Hello")
forward Right "Hello"
backward Right "Hello"
backward Just (Right "Hello")
backward Left (Just (Right "Hello"))

ghci> run $ matchingM leftJustLeft  (Left Nothing)
Left (Left Nothing)
forward Left Nothing
forward Nothing
backward Nothing
backward Left Nothing

ghci> run $ matchingM leftJustLeft  (Right False)
Left (Right False)
forward Right False
backward Right False

ghci> run $ setM leftJustLeft (pure 42) (Left (Just (Left 1)))
Left (Just (Left 42))
forward Left (Just (Left 1))
forward Just (Left 1)
forward Left 1
forward 1
backward 42
backward Left 42
backward Just (Left 42)
backward Left (Just (Left 42))

ghci> run $ setM leftJustLeft (pure 42) (Left Nothing)
Left Nothing
forward Left Nothing
forward Nothing
backward Nothing
backward Left Nothing

-}
