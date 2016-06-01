{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neural.Analytic
    ( Analytic
    , fromDouble
    , fromAnalytic
    , gradient
    ) where

import qualified Numeric.AD.Rank1.Kahn    as K
import qualified Numeric.AD.Internal.Kahn as K

newtype Analytic = Analytic { toKahn :: K.Kahn Double }
    deriving (Show, Num, Eq, Floating, Fractional, Ord, Real, RealFloat, RealFrac)

fromDouble :: Double -> Analytic
fromDouble = Analytic . K.auto

fromAnalytic :: Analytic -> Maybe Double
fromAnalytic x = case toKahn x of
    K.Kahn (K.Lift y) -> Just y
    _                 -> Nothing

gradient :: Traversable t => (Double -> Double -> a) -> (t Analytic -> Analytic) -> t Double -> (Double, t a)
gradient c f = K.gradWith' c f' where
    f' = toKahn . f . fmap Analytic
