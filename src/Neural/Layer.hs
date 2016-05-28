{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Neural.Layer where

import Control.Category
import GHC.TypeLits
import MyPrelude
import Neural.Analytic
import Neural.Layout
import Neural.Matrix
import Neural.Vector
import Prelude          hiding (id, (.))

data LinearLayer (i :: Nat) (o :: Nat) = LinearLayer

instance ( Applicative (Vector (i + 1))
         , Applicative (Matrix o (i + 1))) => Layout (LinearLayer i o) where

    type Source (LinearLayer i o) = Vector i

    type Target (LinearLayer i o) = Vector o

    type Weights (LinearLayer i o) = Matrix o (i + 1)

    initR LinearLayer = sequenceA $ pure $ getRandomR (-0.001, 0.001)

    compute LinearLayer m v = m <%%> (1 :% v)

linearLayer :: ( Applicative (Vector (i + 1))
               , Applicative (Matrix o (i + 1))) => LAYOUT (Vector i) (Vector o)
linearLayer = LAYOUT LinearLayer

layer :: ( Applicative (Vector (i + 1))
         , Applicative (Matrix o (i + 1))) 
         => (forall a. RealFloat a => a -> a) 
         -> LAYOUT (Vector i) (Vector o)
layer f = analytic (fmapAnalytic f) . linearLayer where
