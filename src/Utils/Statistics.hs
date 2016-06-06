{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Utils.Statistics
Description : statistical utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides utilities for working with statistics.
-}

module Utils.Statistics
    ( Probability
    , probability
    , fromProbability
    , countMeanVar
    , mean
    , auc
    , auc'
    , round'
    ) where

import Control.Category ((>>>))
import Control.DeepSeq  (NFData)
import Data.Function    (on)
import Data.List        (sortOn, foldl', partition, groupBy)
import Data.Ord         (Down(..))

-- | A type for representing probabilities.
--
newtype Probability a = Probability { fromProbability :: a } 
    deriving (Show, Read, Eq, Ord, Num, NFData, Functor)

-- | Smart constructor for probabilities.
--
-- >>> probability (0.7 :: Double)
-- Probability {fromProbability = 0.7}
--
-- >>> probability (1.2 :: Double)
-- Probability {fromProbability = 1.0}
--
-- >>> probability (-0.3 :: Double)
-- Probability {fromProbability = 0.0}
--
probability :: RealFloat a => a -> Probability a
probability x
    | x < 0     = Probability 0
    | x > 1     = Probability 1
    | isNaN x   = Probability 0.5
    | otherwise = Probability x

-- | Returns number of elements, mean and variance of a collection of elements.
--
-- >>> countMeanVar [1, 2, 3, 4 :: Float]
-- (4,2.5,1.25)
--
countMeanVar :: forall a. Fractional a => [a] -> (Int, a, a)
countMeanVar xs =
    let (n, s, q) = foldl' f (0, 0, 0) xs
        n'        = fromIntegral n
        m         = s / n'
        v         = q / n' - m * m
    in  (n, m, v)

  where

    f :: (Int, a, a) -> a -> (Int, a, a)
    f (!n, !s, !q) !x = (succ n, s + x, q + x * x)

-- | Calculates the mean of a collection of elements.
--
-- >>> mean [1 .. 5 :: Float]
-- 3.0
--
mean :: forall a. Fractional a => [a] -> a
mean xs =
    let (n, s) = foldl' f (0, 0) xs
        n'        = fromIntegral n
        !m        = s / n'
    in  m

  where

    f :: (Int, a) -> a -> (Int, a)
    f (!n, !s) !x = (succ n, s + x)

-- | Calculates the 
--   <https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve area under the curve>.
--
-- >>> auc [(1, False), (2, True), (3, False), (4, True), (5, False), (6, True), (7, True)]
-- Probability {fromProbability = 0.75}
--
auc :: Ord a => [(a, Bool)] -> Probability Double
auc = probability . auc' . map (\(a, b) -> (a, 1 :: Double, b))

-- | Calculates the 
--   <https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve area under the curve>
--   for /weighted/ samples.
--
-- >>> auc' [(1, (1 :: Double), False), (2, 0.5, True), (3, 1, False), (4, 1, True), (5, 1, False), (6, 1, True), (7, 1, True)]
-- 0.8095238095238095
--
auc' :: forall a b. (Ord a, Fractional b) => [(a, b, Bool)] -> b
auc' xs = let (ps , ns ) = partition third xs
              (ps', ns') = both (normalize . sortOn (Down . fst) . map exceptThird) (ps, ns)
              ns''       = zipWith (\(a, _) (b, b') -> (a, b, b')) ns' $ collate ns'
          in  go 0 ps' ns''

  where

    third :: (c, d, e) -> e
    third (_, _, e) = e

    exceptThird :: (c, d, e) -> (c, d)
    exceptThird (c, d, _) = (c, d)

    both :: (c -> d) -> (c, c) -> (d, d)
    both f (c, c') = (f c, f c')

    normalize :: [(a, b)] -> [(a, b)]
    normalize = f >>> g >>> h

      where

        f ys = let !sb = sum $ map snd ys 
               in  map (\(a, b) -> (a, let !q = b / sb in q)) ys

        g = groupBy ((==) `on` fst)

        h = map (\ys@((a, _) : _) -> (a, sum $ map snd ys))

    collate :: [(a, b)] -> [(b, b)]
    collate = scanr (\(_, b) (b', b'') -> (b, b' + b'')) (0, 0)

    go :: b -> [(a, b)] -> [(a, b, b)] -> b
    go !x []                _                        = x
    go !x _                 []                       = x 
    go !x ps@((a, b) : ps') ns@((a', b', b'') : ns')
        | a > a'                                     = go (x + b * (b' + b''))     ps' ns
        | a == a'                                    = go (x + b * (b' / 2 + b'')) ps' ns' 
        | otherwise                                  = go x                        ps  ns' 

-- | Rounds a 'Double' to the specified number of decimals.
--
-- >>> round' 3 (2/3)
-- 0.667
--
round' :: Int -> Double -> Double
round' d x = let p = 10 ^ d
             in  fromIntegral (round (p * x) :: Integer) / p
