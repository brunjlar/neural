{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Numeric.Neural.Convolution
Description : convolutional layers
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines /convolutional/ layers.
-}

module Numeric.Neural.Convolution
    ( focus 
    , focus'
    ) where

import Data.FixedSize

-- | Focuses on a specific region of a volume.
--
focus :: (KnownNat m, KnownNat n, KnownNat d, KnownNat m', KnownNat n') 
         => a                -- ^ the element to use for out-of-bound indices 
         -> Volume m n d a   -- ^ the original 'Volume'
         -> (Int, Int)       -- ^ the upper left corner of the focused region
         -> Volume m' n' d a
focus x v (i, j) = generate $ \(i', j', k') -> maybe x id $ v !? (i + i', j + j', k')

-- | Specialization of 'focus' to 'Volume's with numeric values,
--   padding out-of-bound indices with zero.
--
-- >>> :set -XDataKinds
-- >>> let v = generate (\(i, j, k) -> i + j + k) :: Volume 2 2 3 Int
-- >>> focus' v (0, 1) :: Volume 1 2 3 Int
-- Volume (Matrix [[[1,2,3],[0,0,0]]])
--
focus' :: (KnownNat m, KnownNat n, KnownNat d, KnownNat m', KnownNat n', Num a) 
          => Volume m n d a   -- ^ the original 'Volume'
          -> (Int, Int)       -- ^ the upper left corner of the focused region
          -> Volume m' n' d a
focus' = focus 0
