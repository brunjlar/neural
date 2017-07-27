{-# LANGUAGE DataKinds #-}

module Data.FixedSize.MatrixSpec (spec) where

import Test.Hspec
import Data.Utils

spec :: Spec
spec = do
    mulSpec
    rowSpec
    columnSpec
    indexSpec
    transposeSpec
    apSpec
    generateSpec

mulSpec :: Spec
mulSpec = describe "(<%%>)" $

    it "should multiply a matrix by a vector" $ do
        let v = generate succ :: Vector 3 Int
        m <%%> v `shouldBe` cons 14 (cons 32 nil)

rowSpec :: Spec
rowSpec = describe "row" $ do

    it "should give the specified row of the matrix if the index is valid" $ do
        row m 0 `shouldBe` (Just $ cons 1 (cons 2 (cons 3 nil)))
        row m 1 `shouldBe` (Just $ cons 4 (cons 5 (cons 6 nil)))

    it "should return Nothing for an invalid row index" $ do
        row m (-1) `shouldBe` Nothing
        row m   2  `shouldBe` Nothing

columnSpec :: Spec
columnSpec = describe "column" $ do

    it "should give the specified column of the matrix if the index is valid" $ do
        column m 0 `shouldBe` (Just $ cons 1 (cons 4 nil))
        column m 2 `shouldBe` (Just $ cons 3 (cons 6 nil))

    it "should return Nothing for an invalid column index" $ do
        column m (-1) `shouldBe` Nothing
        column m   3  `shouldBe` Nothing

indexSpec :: Spec
indexSpec = describe "(!?)" $ do

    it "should give the specified element of the matrix if the index is valid" $ do
        m !? (0, 0) `shouldBe` Just 1
        m !? (1, 2) `shouldBe` Just 6

    it "should return Nothing for an invalid index" $ do
        m !? (2, 0) `shouldBe` Nothing
        m !? (0, 3) `shouldBe` Nothing

transposeSpec :: Spec
transposeSpec = describe "transpose" $

    it "should transpose the matrix" $
        transpose m `shouldBe` generate (\(i, j) -> 3 * j + i + 1)

apSpec :: Spec
apSpec = describe "(<*>)" $

    it "should be component-wise application" $
        (-) <$> m <*> m `shouldBe` pure 0

generateSpec :: Spec
generateSpec = describe "generate" $

    it "should generate a matrix" $ do

        let n = generate id :: Matrix 1 2 (Int, Int)
        show n `shouldBe` "Matrix [[(0,0),(0,1)]]"

m :: Matrix 2 3 Int
m = generate $ \(i, j) -> 3 * i + j + 1 -- 1 2 3
                                        -- 4 5 6
