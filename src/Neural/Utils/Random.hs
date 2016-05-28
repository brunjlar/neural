{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Neural.Utils.Random
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
import           Neural.Utils.List         (pick)

pickR'' :: MonadRandom m => Int -> [a] -> m (a, [a])
pickR'' l xs = do
    i <- getRandomR (0, pred l)
    return $ pick i xs

pickR' :: MonadRandom m => [a] -> m (a, [a])
pickR' xs = pickR'' (length xs) xs

pickR :: MonadRandom m => [a] -> m a
pickR xs = fst <$> pickR' xs

takeR' :: forall m a. MonadRandom m => Int -> [a] -> m ([a], [a])
takeR' n xs = go n (length xs) [] xs

  where

    go :: Int -> Int -> [a] -> [a] -> m ([a], [a])
    go 0  _  ys zs = return (ys, zs)
    go _  0  ys zs = return (ys, zs)
    go !m !l ys zs = do
        (!w, ws) <- pickR'' l zs
        go (pred m) (pred l) (w : ys) ws

takeR :: MonadRandom m => Int -> [a] -> m [a]
takeR n xs = fst <$> takeR' n xs

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

shuffleR :: MonadRandom m => [a] -> m [a]
shuffleR xs = A.elems <$> fisherYates (A.listArray (1, length xs) xs)

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

boxMuller' :: (Floating a, Random a, Eq a, MonadRandom m) => a -> a -> m a
boxMuller' m s = boxMuller >>= \x -> return $ m + s * x

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
