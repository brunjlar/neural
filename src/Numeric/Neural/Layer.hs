{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , logisticLayer
    , softmax
    ) where

import Control.Arrow
import Control.Category
import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import Data.MyPrelude
import Numeric.Neural.Model
import Prelude                 hiding (id, (.))
import Data.Utils.Analytic
import Data.Utils.Matrix
import Data.Utils.Vector

-- | A @'Layer' i o@ is a component that maps a vector of length @i@ to a vector of length @j@.
--
type Layer i o = Component (Vector i Analytic) (Vector o Analytic)

linearLayer' :: ParamFun (Matrix o (i + 1)) (Vector i Analytic) (Vector o Analytic)
linearLayer' = ParamFun $ \xs ws -> ws <%%> cons 1 xs

-- | Creates a /linear/ 'Layer', i.e. a layer that multiplies the input with a weight matrix and adds a bias to get the output.
--
linearLayer :: forall i o. (KnownNat i, KnownNat o) => Layer i o
linearLayer = withNatOp (%+) (Proxy :: Proxy i) (Proxy :: Proxy 1) Component
    { weights = pure 0
    , compute = linearLayer'
    , initR   = sequenceA $ pure $ getRandomR (-0.001, 0.001)
    }

-- | Creates a 'Layer' as a combination of a linear layer and a non-linear activation function.
--
layer :: (KnownNat i, KnownNat o) => (Analytic -> Analytic) -> Layer i o
layer f = arr (fmap f) . linearLayer

-- | This is simply 'layer', specialized to 'tanh'-activation. Output values are all in the interval [0,1].
--
tanhLayer :: (KnownNat i, KnownNat o) => Layer i o
tanhLayer = layer tanh

-- | This is simply 'layer', specialized to the logistic function as activation. Output values are all in the interval [-1,1].
--
logisticLayer :: (KnownNat i, KnownNat o) => Layer i o
logisticLayer = layer $ \x -> 1 / (1 + exp (- x))

-- | The 'softmax' function normalizes a vector, so that all entries are in [0,1] with sum 1. 
--   This means the output entries can be interpreted as probabilities.
--
softmax :: (Floating a, Functor f, Foldable f) => f a -> f a
softmax xs = let xs' = exp <$> xs
                 s   = sum xs'
             in  (/ s) <$> xs'
