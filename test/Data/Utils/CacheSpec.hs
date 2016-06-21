module Data.Utils.CacheSpec (spec) where

import Test.Hspec
import Data.MyPrelude
import Data.Utils

spec :: Spec
spec = cacheSpec

cacheSpec :: Spec
cacheSpec = describe "retrieveC" $ do

    it "should retrieve uncached elements" $
        let w = do
                    (xs, _) <- retrieveC mkCache [1, 2, 3]
                    return xs
        in  runWriter w `shouldBe` ([1, 2, 3], [1, 2, 3])

    it "should use the cache if possible" $
        let w = do
                    (_,  c) <- retrieveC mkCache [1, 2, 3]
                    (xs, _) <- retrieveC c       [3, 3, 1, 4]
                    return xs
        in  runWriter w `shouldBe` ([3, 3, 1, 4], [1, 2, 3, 4])

    it "should respect the cache capacity" $
        let w = do
                    (_,  c ) <- retrieveC mkCache [1, 2, 3]
                    (_,  c') <- retrieveC c       [3, 3, 1, 4]
                    (xs, _ ) <- retrieveC c'      [4, 1]
                    return xs
        in  runWriter w `shouldBe` ([4, 1], [1, 2, 3, 4, 1])

mkCache :: Cache (Writer [Int]) Int Int
mkCache = newCache f 3 where
    f xs = tell xs >> return xs
