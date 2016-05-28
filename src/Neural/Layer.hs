{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Layer
    ( Layer
    , LinearLayer(..)
    , linearLayer
    , layer
    ) where

import Control.Category
import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Witnesses
import MyPrelude
import Neural.Analytic
import Neural.Layout
import Neural.Matrix
import Neural.Vector
import Prelude                 hiding (id, (.))

type Layer i o = LAYOUT (Vector i) (Vector o)

data LinearLayer (i :: Nat) (o :: Nat) = LinearLayer

instance (KnownNat i, KnownNat (i + 1), KnownNat o) => Layout (LinearLayer i o) where

    type Source (LinearLayer i o) = Vector i

    type Target (LinearLayer i o) = Vector o

    type Weights (LinearLayer i o) = Matrix o (i + 1)

    initR LinearLayer = sequenceA $ pure $ getRandomR (-0.001, 0.001)

    compute LinearLayer m v = m <%%> cons 1 v

linearLayer :: forall i o. (KnownNat i, KnownNat o) => Layer i o
linearLayer = withNatOp (%+) (Proxy :: Proxy i) (Proxy :: Proxy 1) $ LAYOUT LinearLayer

layer :: (KnownNat i, KnownNat o) => (forall a. RealFloat a => a -> a) -> Layer i o
layer f = analytic (fmapAnalytic f) . linearLayer
