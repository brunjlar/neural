{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Numeric.Neural.Layer
Description : layer components
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This modules defines special "layer" components and convenience functions for the creation of such layers.
-}

module Numeric.Neural.Layer
    ( Layer
    , linearLayer
    , layer
    , tanhLayer
    , tanhLayer'
    , logisticLayer
    , reLULayer
    , softmax
    ) where

import Control.Category
import Data.FixedSize
import Data.Proxy
import Data.Utils.Analytic
import Data.Utils.Random
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import Numeric.Neural.Model
import Prelude                 hiding (id, (.))

-- | A @'Layer' i o@ is a component that maps a 'Vector' of length @i@ to a 'Vector' of length @o@.
--
type Layer i o = Component (Vector i) (Vector o)

linearLayer' :: forall i o s. Analytic s => ParamFun s (Matrix o (i + 1)) (Vector i s) (Vector o s)
linearLayer' = ParamFun $ \xs ws -> ws <%%> cons 1 xs

-- | Creates a /linear/ 'Layer', i.e. a layer that multiplies the input with a weight 'Matrix' and adds a bias to get the output.
--
--   Random initialization follows the recommendation from chapter 3 of the online book
--   <http://neuralnetworksanddeeplearning.com/ Neural Networks and Deep Learning> by Michael Nielsen.
linearLayer :: forall i o. (KnownNat i, KnownNat o) => Layer i o
linearLayer = withNatOp (%+) p (Proxy :: Proxy 1) Component
    { weights = pure 0
    , compute = linearLayer'
    , initR   = sequenceA $ generate r
    }

  where

    p = Proxy :: Proxy i

    s = 1 / sqrt (fromIntegral $ natVal p)

    r (_, 0) = boxMuller
    r (_, _) = boxMuller' 0 s

-- | Creates a 'Layer' as a combination of a linear layer and a non-linear activation function.
--
layer :: (KnownNat i, KnownNat o) => Diff' -> Layer i o
layer f = cArr (diff f) . linearLayer

-- | This is a simple 'Layer', specialized to 'tanh'-activation. Output values are all in the interval [-1,1].
--
tanhLayer :: (KnownNat i, KnownNat o) => Layer i o
tanhLayer = layer tanh


-- | This is a simple 'Layer', specialized to a modified 'tanh'-activation, following the suggestion from
--   <http://yann.lecun.com/exdb/publis/pdf/lecun-98b.pdf Efficient BackProp> by LeCun et al., where
--   output values are all in the interval [-1.7159,1.7159].
tanhLayer' :: (KnownNat i, KnownNat o) => Layer i o
tanhLayer' = layer $ \x -> 1.7159 * tanh (2 * x / 3)

-- | This is a simple 'Layer', specialized to the logistic function as activation. Output values are all in the interval [0,1].
--
logisticLayer :: (KnownNat i, KnownNat o) => Layer i o
logisticLayer = layer $ \x -> 1 / (1 + exp (- x))

-- | This is a simple 'Layer', specialized to the /rectified linear unit/ activation function.
--   Output values are all non-negative.
--
reLULayer :: (KnownNat i, KnownNat o) => Layer i o
reLULayer = layer $ \x -> max 0 x

-- | The 'softmax' function normalizes a vector, so that all entries are in [0,1] with sum 1.
--   This means the output entries can be interpreted as probabilities.
--
softmax :: (Floating a, Functor f, Foldable f) => f a -> f a
softmax xs = let xs' = exp <$> xs
                 s   = sum xs'
             in  (/ s) <$> xs'
