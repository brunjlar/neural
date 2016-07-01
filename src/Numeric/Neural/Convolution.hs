{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , cover
    , cover'
    ) where

import Data.FixedSize
import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Witnesses

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

-- | Covers a 'Volume' with (smaller) regions.
--
cover :: forall m n d a s m' n'. 
         (KnownNat m, KnownNat n, KnownNat d, KnownNat s, KnownNat m', KnownNat n')
         => a              -- ^ the element to use for out-of-bound indices
         -> Volume m n d a -- ^ the original 'Volume'
         -> Proxy s        -- ^ a proxy to the region size
         -> Int            -- ^ the stride
         -> Matrix m' n' (Vector (s * s * d) a)
cover x v ps stride = withNatOp (%*) ps ps $ 
                      withNatOp (%*) (Proxy :: Proxy (s * s)) (Proxy :: Proxy d) $
                      generate $ toVector . volume

  where volume :: (Int, Int) -> Volume s s d a 
        volume (i, j) = focus x v (stride * i, stride * j)

-- | Specialization of 'cover' to 'Volume's with numeric values.
--
-- >>> :set -XDataKinds
-- >>> let v = generate (\(i, j, _) -> i + j) :: Volume 4 4 1 Int
-- >>> cover' v (Proxy :: Proxy 3) 1 :: Matrix 2 2 (Vector 9 Int)
-- Matrix [[[0,1,2,1,2,3,2,3,4],[1,2,3,2,3,4,3,4,5]],[[1,2,3,2,3,4,3,4,5],[2,3,4,3,4,5,4,5,6]]]
--
cover' :: forall m n d a s m' n'. 
         (KnownNat m, KnownNat n, KnownNat d, KnownNat s, KnownNat m', KnownNat n', Num a)
         => Volume m n d a -- ^ the original 'Volume'
         -> Proxy s        -- ^ a proxy to the region size 
         -> Int            -- ^ the stride
         -> Matrix m' n' (Vector (s * s * d) a)
cover' = cover 0
