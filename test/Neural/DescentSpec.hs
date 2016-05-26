module Neural.DescentSpec (spec) where

import Neural
import Test.Hspec

spec :: Spec
spec = do
    addSpec

addSpec :: Spec
addSpec = describe "add" $

    it "should sum 1 and 1 to 2" $
        ((1 :: Int) + 1) `shouldBe` 2
