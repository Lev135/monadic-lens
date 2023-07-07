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
  let getter = toM (\s -> log' s $> fst s)
      setter = settingM \k (a, x) -> do
          log "before" (a, x)
          a' <- k a
          log "after" (a', x)
          pure (a', x)
      mval42 = n42 <$ tell ["42"]
      lens = lensM (\(a, b) -> log2 "getting" a "from" (a, b) $> a)
                  (\(a, b) a' -> log2 "setting" a' "into" (__ a, b) $> (a', b))
      prism :: forall a b. (Show a, Show b) => PrismM' (Writer Log) (Sum a b) a
      prism = prismM (\a -> log2 "building" (L @_ @b a) "from" a $> L a) \case
              L a -> log2 "succeed match" a "from" (L @_ @b a) $> Right a
              R a -> log "failed match from" (R @a a) $> Left (R a)

  context "getter" do
    it "view" $
      run (viewM (getter . getter) ((n1, n2), n3))
        `shouldBe` (n1, ["((1,2),3)", "(1,2)"])
  context "setter" do
    it "set" $
      run (setM (setter . setter) mval42 ((n1, n2), n3))
        `shouldBe` (((n42, n2), n3),
          [ "before ((1,2),3)"
          , "before (1,2)"
          , "42"
          , "after (42,2)"
          , "after ((42,2),3)"
          ])
  context "lens" do
    let l = lens . lens
    it "view" $
      run (viewM l ((n1, n2), n3))
        `shouldBe` (n1, ["getting (1,2) from ((1,2),3)", "getting 1 from (1,2)"])
    it "set" $
      run (setM l mval42 ((n1, n2), n3))
        `shouldBe` (((n42, n2), n3),
          [ "getting (1,2) from ((1,2),3)"
          , "getting 1 from (1,2)"
          , "42"
          , "setting 42 into (_1,2)"
          , "setting (42,2) into (_(1,2),3)"
          ])
  context "prisms" do
    let l = prism . prism
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
    it "matching failed 2" $
      run (matchingM l test_failed2)
        `shouldBe` (Left (L (R n1)),
          [ "succeed match R 1 from L (R 1)"
          , "failed match from R 1"
          , "building L (R 1) from R 1"
          ])
    it "set succeed" $
      run (setM l mval42 test_succeed)
        `shouldBe` (L (L n42),
          [ "succeed match L 1 from L (L 1)"
          , "succeed match 1 from L 1"
          , "42"
          , "building L 42 from 42"
          , "building L (L 42) from L 42"
          ])
    it "set failed 1" $
      run (setM l mval42 test_failed1)
        `shouldBe` (R n1, ["failed match from R 1"])
    it "set failed 2" $
      run (setM l mval42 test_failed2)
        `shouldBe` (L (R n1),
          [ "succeed match R 1 from L (R 1)"
          , "failed match from R 1"
          , "building L (R 1) from R 1"
          ])
    it "preview succeed" $
      run (previewM l test_succeed)
        `shouldBe` (Just n1,
          [ "succeed match L 1 from L (L 1)"
          , "succeed match 1 from L 1"
          ])
    it "preview failed 1" $
      run (previewM l test_failed1)
        `shouldBe` (Nothing, ["failed match from R 1"])
    it "preview failed 2" $
      run (previewM l test_failed2)
        `shouldBe` (Nothing,
          [ "succeed match R 1 from L (R 1)"
          , "failed match from R 1"
          ])
  context "lens . prism . prism" do
    let l = lens . prism . prism
        test_succeed = (L (L n1), n2) :: (Sum (Sum Int Int) Int, Int)
        test_failed1 = (R    n1,  n2) :: (Sum (Sum Int Int) Int, Int)
        test_failed2 = (L (R n1), n2) :: (Sum (Sum Int Int) Int, Int)
    it "preview succeed" $
      run (previewM l test_succeed)
        `shouldBe` (Just n1,
          [ "getting L (L 1) from (L (L 1),2)"
          , "succeed match L 1 from L (L 1)"
          , "succeed match 1 from L 1"
          ])
    it "preview failed 1" $
      run (previewM l test_failed1)
        `shouldBe` (Nothing,
          [ "getting R 1 from (R 1,2)"
          , "failed match from R 1"
          ])
    it "preview failed 2" $
      run (previewM l test_failed2)
        `shouldBe` (Nothing,
          [ "getting L (R 1) from (L (R 1),2)"
          , "succeed match R 1 from L (R 1)"
          , "failed match from R 1"
          ])
    it "set succeed" $
      run (setM l mval42 test_succeed)
        `shouldBe` ((L $ L 42, 2),
          [ "getting L (L 1) from (L (L 1),2)"
          , "succeed match L 1 from L (L 1)"
          , "succeed match 1 from L 1"
          , "42"
          , "building L 42 from 42"
          , "building L (L 42) from L 42"
          , "setting L (L 42) into (_L (L 1),2)"
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
