{-# LANGUAGE DataKinds #-}

module Data.FixedSize.ClassSpec (spec) where

import Test.Hspec
import Data.Utils

spec :: Spec
spec = indexSpec

indexSpec :: Spec
indexSpec = describe "(!)" $ do

    let v = cons 'x' nil
        m = generate id :: Matrix 1 2 (Int, Int)
        o = generate id :: Volume 1 2 3 (Int, Int, Int)

    it "should give the specified element if the index is valid" $ do
        v ! 0 `shouldBe` 'x'
        m ! (0, 1) `shouldBe` (0, 1)
        o ! (0, 1, 2) `shouldBe` (0, 1, 2)


    it "should throw an exception if the index is invalid" $ do
        print (v ! 1) `shouldThrow` anyErrorCall
        print (m ! (0, 2)) `shouldThrow` anyErrorCall
        print (o ! (0, 1, 3)) `shouldThrow` anyErrorCall
