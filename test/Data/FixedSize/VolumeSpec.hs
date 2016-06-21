{-# LANGUAGE DataKinds #-}

module Data.FixedSize.VolumeSpec (spec) where

import Test.Hspec
import Data.Utils

spec :: Spec
spec = do
    indexSpec
    generateSpec

indexSpec :: Spec
indexSpec = describe "(!?)" $ do

    let v = generate id :: Volume 1 2 3 (Int, Int, Int)

    it "should give the specified element of the volume if the index is valid" $ do
        v !? (0, 0, 0) `shouldBe` Just (0, 0, 0) 
        v !? (0, 1, 2) `shouldBe` Just (0, 1, 2) 

    it "should return Nothing for an invalid index" $ do
        v !? (0, 0, 3) `shouldBe` Nothing
        v !? (1, 0, 0) `shouldBe` Nothing

generateSpec :: Spec
generateSpec = describe "generate" $

    it "should generate a volume" $ do

        let v =  generate id :: Volume 1 1 3 (Int, Int, Int)
        show v `shouldBe` "Volume (Matrix [[[(0,0,0),(0,0,1),(0,0,2)]]])"
