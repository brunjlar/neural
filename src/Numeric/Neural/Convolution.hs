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
    , convolution
    ) where

import Control.Category
import Data.FixedSize
import Data.Proxy
import Data.Utils
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import Numeric.Neural.Layer
import Numeric.Neural.Model
import Prelude                hiding (id, (.))

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
         -> Proxy s        -- ^ a proxy to the region size
         -> Int            -- ^ the stride
         -> Volume m n d a -- ^ the original 'Volume'
         -> Matrix m' n' (Vector (s * s * d) a)
cover x ps stride v = withNatOp (%*) ps ps $ 
                      withNatOp (%*) (Proxy :: Proxy (s * s)) (Proxy :: Proxy d) $
                      generate $ toVector . volume

  where volume :: (Int, Int) -> Volume s s d a 
        volume (i, j) = focus x v (stride * i, stride * j)

-- | Specialization of 'cover' to 'Volume's with numeric values.
--
-- >>> :set -XDataKinds
-- >>> let v = generate (\(i, j, _) -> i + j) :: Volume 4 4 1 Int
-- >>> cover' (Proxy :: Proxy 3) 1 v :: Matrix 2 2 (Vector 9 Int)
-- Matrix [[[0,1,2,1,2,3,2,3,4],[1,2,3,2,3,4,3,4,5]],[[1,2,3,2,3,4,3,4,5],[2,3,4,3,4,5,4,5,6]]]
--
cover' :: forall m n d a s m' n'. 
         (KnownNat m, KnownNat n, KnownNat d, KnownNat s, KnownNat m', KnownNat n', Num a)
         => Proxy s        -- ^ a proxy to the region size 
         -> Int            -- ^ the stride
         -> Volume m n d a -- ^ the original 'Volume'
         -> Matrix m' n' (Vector (s * s * d) a)
cover' = cover 0

-- | Convolves a 'Layer' over a 'Volume'.
--
convolution :: forall s m n d m' n' d'. 
               (KnownNat s, KnownNat m, KnownNat n, KnownNat d, KnownNat m', KnownNat n') 
               => Proxy s              -- ^ a proxy to the region size
               -> Int                  -- ^ the stride
               -> Layer (s * s * d) d' -- ^ the layer to convolve
               -> Component (Volume m n d) (Volume m' n' d')
convolution ps stride l = cArr (Diff $ toVolume . unConvolve) .
                          cConvolve l .
                          cArr (Diff $ Convolve . cover' ps stride)

  where

    _ = natVal (Proxy :: Proxy d)
