{-# LANGUAGE DataKinds #-}

module Utils.VectorSpec (spec) where

import Test.Hspec
import Data.Utils

spec :: Spec
spec = do
    indexSpec
    generateSpec
    spSpec
    vheadSpec
    vtailSpec
    apSpec

indexSpec :: Spec
indexSpec = describe "(!?)" $ do

    it "should give the element at a specified index if that index is valid" $
        cons 1 (cons 2 nil) !? 1 `shouldBe` Just (2 :: Int)

    it "should return Nothing if the index is not valid" $
        cons 1 (cons 2 nil) !? 2 `shouldBe` (Nothing :: Maybe Int)

generateSpec :: Spec
generateSpec = describe "generate" $

    it "should generate a vector" $
        generate id `shouldBe` cons 0 (cons 1 (cons 2 nil))

spSpec :: Spec
spSpec = describe "(<%>)" $

    it "should compute the scalar product of two vectors" $ do
        let v = cons 1 (cons 2 nil)
            w = cons 3 (cons 4 nil)
        v <%> w `shouldBe` (11 :: Int)

vheadSpec :: Spec
vheadSpec = describe "vhead" $

    it "should give the head of a vector of positive length" $
        vhead (cons 1 (cons 2 nil)) `shouldBe` (1 :: Int)

vtailSpec :: Spec
vtailSpec = describe "vtail" $

    it "should give the tail of a vector of positive length" $
        vtail (cons 1 (cons 2 nil)) `shouldBe` (cons 2 nil :: Vector 1 Int)

apSpec :: Spec
apSpec = describe "(<*>)" $

    it "should be component-wise application" $ do
        let v = cons 1 (cons 2 nil) :: Vector 2 Int
        (+) <$> v <*> v `shouldBe` ((* 2) <$> v)
