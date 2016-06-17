{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Numeric.Neural.Normalization
Description : normalizing data
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This modules provides utilities for data normalization.
-}

module Numeric.Neural.Normalization
    ( encode1ofN
    , decode1ofN
    , encodeEquiDist
    , decodeEquiDist
    , crossEntropyError
    , white
    , whiten
    , Classifier
    , mkStdClassifier
    ) where

import Control.Arrow          (first)
import Control.Category
import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import Data.MyPrelude
import Data.Utils.Analytic
import Data.Utils.Statistics
import Data.Utils.Traversable
import Data.Utils.Vector
import Numeric.Neural.Layer
import Numeric.Neural.Model
import Prelude                hiding (id, (.))

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
decode1ofN :: (Enum a, Ord b, Foldable f) => f b -> a
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
-- >>> runDiff (crossEntropyError LT) (cons 0.8 (cons 0.1 (cons 0.1 nil))) :: Identity Double
-- Identity 0.2231435513142097
--
-- >>> runDiff (crossEntropyError EQ) (cons 0.8 (cons 0.1 (cons 0.1 nil))) :: Identity Double
-- Identity 2.3025850929940455
--
crossEntropyError :: (Enum a, KnownNat n) => a -> Diff (Vector n) Identity
crossEntropyError a = Diff $ \ys -> Identity $ negate $ log $ (\y -> 0.98 * y + 0.01) $ encode1ofN a <%> ys

-- | Function 'white' takes a batch of values (of a specific shape)
--   and computes a normalization function which whitens values of that shape,
--   so that each component has zero mean and unit variance.
--
-- >>> :set -XDataKinds
-- >>> let xss = [cons 1 (cons 1 nil), cons 1 (cons 2 nil), cons 1 (cons 3 nil)] :: [Vector 2 Double]
-- >>> let f   = white xss
-- >>> f <$> xss
-- [[0.0,-1.2247448713915887],[0.0,0.0],[0.0,1.2247448713915887]]
white :: (Applicative f, Traversable t, Eq a, Floating a) => t (f a) -> f a -> f a
white xss = ((w <$> sequenceA xss) <*>) where

    w xs = case toList xs of
        []  -> id
        xs' -> let (_, !m, !v) = countMeanVar xs'
                   !s          = if v == 0 then 1 else 1 / sqrt v
               in  \x -> (x - m) * s

-- | Modifies a 'Model' by whitening the input before feeding it into the embedded component.
--
whiten :: (Applicative f, Traversable t)
          => Model f g a b c             -- ^ original model 
          -> t b                         -- ^ batch of input data
          -> Model f g a b c             
whiten (Model c e i o) xss = Model c e' i' o where

    w = white $ i <$> xss

    i' = w . i

    e' = first w . e

-- | A @'Classifier' f n b c@ is a 'Model' that classifies items of type @b@ into categories of type @c@,
--   using a component with input shape @f@ and output shape @'Vector' n@.
--
type Classifier f n b c = StdModel f (Vector n) b c

-- | Makes a standard 'Classifier' which uses a softmax layer, "1 of n" encoding and the cross entropy error.
--
mkStdClassifier :: (Functor f, KnownNat n, Enum c)
                   => Component f (Vector n) -- ^ the embedded component
                   -> (b -> f Double)        -- ^ converts input
                   -> Classifier f n b c
mkStdClassifier c i = mkStdModel (cArr (Diff softmax) . c) crossEntropyError i decode1ofN
