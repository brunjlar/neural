{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neural.Analytic
    ( Analytic
    , fromDouble
    , gradient
    ) where

import qualified Numeric.AD.Rank1.Kahn as K

newtype Analytic = Analytic { toKahn :: K.Kahn Double }
    deriving (Num, Eq, Floating, Fractional, Ord, Real, RealFloat, RealFrac)

fromDouble :: Double -> Analytic
fromDouble = Analytic . K.auto

gradient :: Traversable t => (Double -> Double -> a) -> (t Analytic -> Analytic) -> t Double -> (Double, t a)
gradient c f = K.gradWith' c f' where
    f' = toKahn . f . fmap Analytic
