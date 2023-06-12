{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}

module Main (main) where

import Control.Lens.Monadic
import Control.Monad.Writer (MonadWriter(..), Writer, runWriter)
import Data.Functor (($>))
import Prelude hiding (log)
import Test.Hspec

type Log = [String]

effectsOrderSpec :: Spec
effectsOrderSpec = do
  let log2 c a c' b = tell [unwords[c, show a, c', show b]]
      log c a = tell [c ++ " " ++ show a]
      log'  a = tell [show a]
      run = runWriter @Log
      (n1, n2, n3, n42) = (1, 2, 3, 42) :: (Int, Int, Int, Int)

  context "getter" do
    let f = toM (\s -> log' s $> fst s)
    it "view" $
      run (viewM (f . f) ((n1, n2), n3))
        `shouldBe` (n1, ["((1,2),3)", "(1,2)"])
  context "setter" do
    let f = setsM \k (a, x) -> do
              log "before" (a, x)
              a' <- k a
              log "after" (a', x)
              pure (a', x)
    it "set" $
      run (setM (f . f) (pure n42) ((n1, n2), n3))
        `shouldBe` (((n42, n2), n3),
          ["before ((1,2),3)", "before (1,2)", "after (42,2)", "after ((42,2),3)"])
  context "lens" do
    let f = lensM (\(a, b) -> log2 "getting" a "from" (a, b) $> a)
                  (\(a, b) a' -> log2 "setting" a' "into" (__ a, b) $> (a', b))
        l = f . f
    it "view" $
      run (viewM l ((n1, n2), n3))
        `shouldBe` (n1, ["getting (1,2) from ((1,2),3)", "getting 1 from (1,2)"])
    it "set" $
      run (setM l (pure n42) ((n1, n2), n3))
        `shouldBe` (((n42, n2), n3),
          [ "getting (1,2) from ((1,2),3)"
          , "getting 1 from (1,2)"
          , "setting 42 into (_1,2)"
          , "setting (42,2) into (_(1,2),3)"
          ])
  context "prisms" do
    let f :: forall a b. (Show a, Show b) => PrismM' (Writer Log) (Sum a b) a
        f = prismM (\a -> log2 "building" (L @_ @b a) "from" a $> L a) \case
              L a -> log2 "succeed match" a "from" (L @_ @b a) $> Right a
              R a -> log "failed match from" (R @a a) $> Left (R a)
        l = f . f
        test_succeed = L (L n1) :: Sum (Sum Int Int) Int
        test_failed1 = R n1 :: Sum (Sum Int Int) Int
        test_failed2 = L (R n1) :: Sum (Sum Int Int) Int
    it "matching succeed" $
      run (matchingM l test_succeed)
        `shouldBe` (Right n1,
          [ "succeed match L 1 from L (L 1)"
          , "succeed match 1 from L 1"
          ])
    it "matching failed 1" $
      run (matchingM l test_failed1)
        `shouldBe` (Left (R n1), ["failed match from R 1"])
    it "matching succeed 2" $
      run (matchingM l test_failed2)
        `shouldBe` (Left (L (R n1)),
          [ "succeed match R 1 from L (R 1)"
          , "failed match from R 1"
          , "building L (R 1) from R 1"      -- it's a very unexpected behavior!
          ])
    it "set succeed" $
      run (setM l (pure n42) test_succeed)
        `shouldBe` (L (L n42),
          [ "succeed match L 1 from L (L 1)"
          , "succeed match 1 from L 1"
          , "building L 42 from 42"
          , "building L (L 42) from L 42"
          ])
    it "set failed 1" $
      run (setM l (pure n42) test_failed1)
        `shouldBe` (R n1, ["failed match from R 1"])
    it "set failed 2" $
      run (setM l (pure n42) test_failed2)
        `shouldBe` (L (R n1),
          [ "succeed match R 1 from L (R 1)"
          , "failed match from R 1"
          , "building L (R 1) from R 1"
          ])

data Sum a b
  = L a
  | R b
  deriving (Eq, Show)

newtype Underscored a
  = Underscored a

__ :: a -> Underscored a
__ = Underscored

instance Show a => Show (Underscored a) where
  show (Underscored a) = "_" ++ show a

main :: IO ()
main = hspec do
  effectsOrderSpec
