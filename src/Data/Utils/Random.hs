{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Data.Utils.Random
Description : random number utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides utilities for working with module 'Control.Monad.Random'.
-}

module Data.Utils.Random
    ( pickR'
    , pickR
    , takeR'
    , takeR
    , fisherYates
    , shuffleR
    , boxMuller
    , boxMuller'
    , roulette
    ) where

import           Control.Monad             (forM_, unless, replicateM)
import           Control.Monad.Random
import qualified Control.Monad.ST.Trans    as ST
import           Control.Monad.Trans.Class (lift)
import qualified Data.Array                as A
import           Data.List                 (mapAccumL)
import           Data.Utils.List           (pick)

pickR'' :: MonadRandom m => Int -> [a] -> m (a, [a])
pickR'' l xs = do
    i <- getRandomR (0, pred l)
    return $ pick i xs

-- | Picks a random element of the list and returns that element and the remaining elements.
--
-- >>> evalRand (pickR' "Haskell") (mkStdGen 4712)
-- ('s',"Hakell")
--
pickR' :: MonadRandom m => [a] -> m (a, [a])
pickR' xs = pickR'' (length xs) xs

-- | Picks a random element of the list.
--
-- >>> evalRand (pickR "Haskell") (mkStdGen 4712)
-- 's'
--
pickR :: MonadRandom m => [a] -> m a
pickR xs = fst <$> pickR' xs

-- | Takes the specified number of random elements from the list.
--   Returns those elements and the remaining elements.
--
-- >>> evalRand (takeR' 3 "Haskell") (mkStdGen 4712)
-- ("aks","Hell")
--
takeR' :: forall m a. MonadRandom m => Int -> [a] -> m ([a], [a])
takeR' n xs = go n (length xs) [] xs

  where

    go :: Int -> Int -> [a] -> [a] -> m ([a], [a])
    go 0  _  ys zs = return (ys, zs)
    go _  0  ys zs = return (ys, zs)
    go !m !l ys zs = do
        (!w, ws) <- pickR'' l zs
        go (pred m) (pred l) (w : ys) ws

-- | Takes the specified number of random elements from the list.
--
-- >>> evalRand (takeR 3 "Haskell") (mkStdGen 4712)
-- "aks"
--
takeR :: MonadRandom m => Int -> [a] -> m [a]
takeR n xs = fst <$> takeR' n xs

-- | Shuffles an array with the
--   < https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates algorithm>.
--
fisherYates :: forall m a. MonadRandom m => A.Array Int a -> m (A.Array Int a)
fisherYates a = ST.runSTArray st where

    st :: forall s. ST.STT s m (ST.STArray s Int a)
    st = do
        let (m, n) = A.bounds a
        b <- ST.thawSTArray a
        forM_ [m .. pred n] $ \i -> do
            j <- lift $ getRandomR (i, n)
            unless (i == j) $ do
                x <- ST.readSTArray b i
                y <- ST.readSTArray b j
                ST.writeSTArray b i y
                ST.writeSTArray b j x
        return b

-- | Shuffles an list with the
--   < https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle Fisher-Yates algorithm>.
--
-- >>> evalRand (shuffleR "Haskell") (mkStdGen 4712)
-- "skalHle"
--
shuffleR :: MonadRandom m => [a] -> m [a]
shuffleR xs = A.elems <$> fisherYates (A.listArray (1, length xs) xs)

-- | Uses the <https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform Box-Muller transform>
--   to sample the standard normal distribution (zero expectation, unit variance).
--
-- >>> evalRand (replicateM 5 boxMuller) (mkStdGen 1234) :: [Float]
-- [0.61298496,-0.19325614,4.4974413e-2,-0.31926495,-1.1109064]
--
boxMuller :: forall m a. (Floating a, Random a, Eq a, MonadRandom m) => m a
boxMuller = do
    u1 <- u
    u2 <- u
    return $ sqrt (-2 * log u1) * cos (2 * pi * u2)

  where

    u :: m a
    u = do
        x <- getRandomR (0, 1)
        if x == 0 then u else return x

-- | Uses the <https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform Box-Muller transform>
--   to sample a normal distribution with specified mean and stadard deviation.
--
-- >>> evalRand (replicateM 5 $ boxMuller' 10 2) (mkStdGen 1234) :: [Float]
-- [11.22597,9.613487,10.089949,9.36147,7.7781873]
--
boxMuller' :: (Floating a, Random a, Eq a, MonadRandom m) => a -> a -> m a
boxMuller' m s = boxMuller >>= \x -> return $ m + s * x

-- | Randomly selects the specified number of elements of a /weighted/ list.
--
-- >>> evalRand (roulette 10 [('x', 1 :: Double), ('y', 2)]) (mkStdGen 1000)
-- "yxxyyyyxxy"
--
roulette :: forall a b m. (Ord b, Fractional b, Random b, MonadRandom m) => Int -> [(a, b)] -> m [a]
roulette n xs = do
    let (!s, ys) = mapAccumL f 0 xs
        zs       = map (\(a, b) -> (a, b / s)) ys
    replicateM n $ g zs <$> getRandomR (0, 1) 

  where

    f :: b -> (a, b) -> (b, (a, b))
    f s (a, w) = let !s' = s + w in (s', (a, s'))

    g :: [(a, b)] -> b -> a
    g []             _ = error "empty list"
    g [(a, _)]       _ = a
    g ((a, b') : ws) b
        | b <= b'      = a
        | otherwise    = g ws b
