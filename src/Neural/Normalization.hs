{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Neural.Normalization
Description : normalizing data
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This modules provides utilities for data normalization.
-}

module Neural.Normalization
    ( encode1ofN
    , decode1ofN
    , encodeEquiDist
    , decodeEquiDist
    , crossEntropyError
    ) where

import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import MyPrelude
import Utils.Traversable
import Utils.Vector

-- | Provides "1 of @n@" encoding for enumerable types.
--
-- >>> :set -XDataKinds
-- >>> encode1ofN LT :: Vector 3 Int
-- [1,0,0]
--
-- >>> encode1ofN EQ :: Vector 3 Int
-- [0,1,0]
--
-- >>> encode1ofN GT :: Vector 3 Int
-- [0,0,1]
--
encode1ofN :: (Enum a, Num b, KnownNat n) => a -> Vector n b
encode1ofN x = generate $ \i -> if i == fromEnum x then 1 else 0

-- | Provides "1 of @n@" decoding for enumerable types.
--
-- >>> decode1ofN [0.9, 0.3, 0.1 :: Double] :: Ordering
-- LT
--
-- >>> decode1ofN [0.7, 0.8, 0.6 :: Double] :: Ordering
-- EQ
--
-- >>> decode1ofN [0.2, 0.3, 0.8 :: Double] :: Ordering
-- GT
--
decode1ofN :: (Enum a, Num b, Ord b, Foldable f) => f b -> a
decode1ofN = toEnum . fst . maximumBy (compare `on` snd) . zip [0..] . toList

polyhedron :: Floating a => Int -> [[a]]
polyhedron = fst . p

  where

    p 2 = ([[-1], [1]], 2)
    p n = let (xs, d) = p (n - 1)
              y       = sqrt (d * d - 1)
              v       = y : replicate (n - 2) 0
              xs'     = v : ((0 :) <$> xs)
              shift   = y / fromIntegral n
              shifted = (\(z : zs) -> (z - shift : zs)) <$> xs'
              scale   = 1 / (y - shift)
              scaled  = ((scale *) <$>) <$> shifted
          in  (scaled, d * scale)

polyhedron' :: forall a n. (Floating a, KnownNat n) => Proxy n -> [[a]]
polyhedron' p = withNatOp (%+) p (Proxy :: Proxy 1) $
    polyhedron (fromIntegral $ natVal (Proxy :: Proxy (n + 1)))

-- | Provides equidistant encoding for enumerable types.
--
-- >>> :set -XDataKinds
-- >>> encodeEquiDist LT :: Vector 2 Float
-- [1.0,0.0]
--
-- >>> encodeEquiDist EQ :: Vector 2 Float
-- [-0.5,-0.86602545]
--
-- >>> encodeEquiDist GT :: Vector 2 Float
-- [-0.5,0.86602545]
--
encodeEquiDist :: forall a b n. (Enum a, Floating b, KnownNat n) => a -> Vector n b
encodeEquiDist x = let ys = polyhedron' (Proxy :: Proxy n)
                       y  = ys !! fromEnum x
                   in  fromJust (fromList y)

-- | Provides equidistant decoding for enumerable types.
--
-- >>> :set -XDataKinds
-- >>> let u = fromJust (fromList [0.9, 0.2]) :: Vector 2 Double
-- >>> decodeEquiDist u :: Ordering
-- LT
--
-- >>> :set -XDataKinds
-- >>> let v = fromJust (fromList [-0.4, -0.5]) :: Vector 2 Double
-- >>> decodeEquiDist v :: Ordering
-- EQ
--
-- >>> :set -XDataKinds
-- >>> let w = fromJust (fromList [0.1, 0.8]) :: Vector 2 Double
-- >>> decodeEquiDist w :: Ordering
-- GT
--
decodeEquiDist :: forall a b n. (Enum a, Ord b, Floating b, KnownNat n) => Vector n b -> a
decodeEquiDist y = let xs  = polyhedron' (Proxy :: Proxy n)
                       xs' = (fromJust . fromList) <$> xs
                       ds  = [(j, sqDiff x y) | (j, x) <- zip [0..] xs']
                       i   = fst $ minimumBy (compare `on` snd) ds
                   in  toEnum i

-- | Computes the cross entropy error (assuming "1 of n" encoding).
--
-- >>> crossEntropyError LT (cons 0.8 (cons 0.1 (cons 0.1 nil))) :: Float
-- 0.22314353
--
-- >>> crossEntropyError EQ (cons 0.8 (cons 0.1 (cons 0.1 nil))) :: Float
-- 2.3025851 
--
crossEntropyError :: (Enum a, Floating b, KnownNat n) => a -> Vector n b -> b
crossEntropyError a ys = negate $ log $ encode1ofN a <%> ys
