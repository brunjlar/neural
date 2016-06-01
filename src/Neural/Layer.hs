{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Layer
    ( Layer
    , linearLayer
    , layer
    , tanhLayer
    ) where

import Control.Arrow
import Control.Category
import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import MyPrelude
import Neural.Component
import Prelude                 hiding (id, (.))
import Utils.Analytic
import Utils.Matrix
import Utils.Vector

type Layer i o = Component (Vector i Analytic) (Vector o Analytic)

linearLayer' :: Component' (Matrix o (i + 1)) (Vector i Analytic) (Vector o Analytic)
linearLayer' = Component' $ \xs ws -> ws <%%> cons 1 xs

linearLayer :: forall i o. (KnownNat i, KnownNat o) => Layer i o
linearLayer = withNatOp (%+) (Proxy :: Proxy i) (Proxy :: Proxy 1) Component
    { weights = pure 0
    , compute = linearLayer'
    , initR   = sequenceA $ pure $ getRandomR (-0.001, 0.001)
    }

layer :: (KnownNat i, KnownNat o) => (Analytic -> Analytic) -> Layer i o
layer f = arr (fmap f) . linearLayer

tanhLayer :: (KnownNat i, KnownNat o) => Layer i o
tanhLayer = layer tanh
