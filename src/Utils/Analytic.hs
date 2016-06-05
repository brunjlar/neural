{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Utils.Analytic
Description : "analytic" values
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the numeric type 'Analytic', which has "built in differentiation".
-}

module Utils.Analytic
    ( Analytic
    , fromDouble
    , fromAnalytic
    , gradient
    ) where

import qualified Numeric.AD.Rank1.Kahn    as K
import qualified Numeric.AD.Internal.Kahn as K

-- | The numeric type 'Analytic' is a wrapper around Edward Kmett's @'K.Kahn' Double@ type.
--   Using functions from Analytics to Analytics, we automatically get numerically exact gradients.
--   An number of type 'Analytic' is conceptionally a 'Double' together with an infinitesimal component.
--
newtype Analytic = Analytic { toKahn :: K.Kahn Double }
    deriving (Show, Num, Eq, Floating, Fractional, Ord, Real, RealFloat, RealFrac)

-- | Converts a 'Double' to an 'Analytic' without infinitesimal component.
--
fromDouble :: Double -> Analytic
fromDouble = Analytic . K.auto

-- | Tries to convert an 'Analytic' to a 'Double'.
--   This conversion will work if the 'Analytic' has no infinitesimal component.
--
fromAnalytic :: Analytic -> Maybe Double
fromAnalytic x = case toKahn x of
    K.Kahn (K.Lift y) -> Just y
    _                 -> Nothing

-- | Computes the gradient of an analytic function and combines it with the argument. 
--
-- >>> gradient (\_ d -> d) (\[x, y] -> x * x + 3 * y + 7) [2, 1]
-- (14.0,[4.0,3.0])
--
gradient :: Traversable t 
            => (Double -> Double -> a)  -- ^ how to combine argument and gradient
            -> (t Analytic -> Analytic) -- ^ analytic function 
            -> t Double                 -- ^ function argument
            -> (Double, t a)            -- ^ function value and combination of argument and gradient
gradient c f = K.gradWith' c f' where
    f' = toKahn . f . fmap Analytic
