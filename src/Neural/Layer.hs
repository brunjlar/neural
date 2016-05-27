{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Neural.Layer
    ( LinearLayer(..)
    , layer
    ) where

import GHC.TypeLits
import MyPrelude
import Neural.Layout
import Neural.Matrix
import Neural.Vector

data LinearLayer (i :: Nat) (o :: Nat) = LinearLayer

instance ( Applicative (Vector (i + 1))
         , Applicative (Matrix o (i + 1))) => Layout (LinearLayer i o) where

    type Source (LinearLayer i o) = Vector i

    type Target (LinearLayer i o) = Vector o

    type Weights (LinearLayer i o) = Matrix o (i + 1)

    initR LinearLayer = sequenceA $ pure $ getRandomR (-0.001, 0.001)

    compute LinearLayer m v = m <%%> (1 :% v)

layer :: ( Applicative (Vector (i + 1))
         , Applicative (Matrix o (i + 1))) 
         => (forall a. RealFloat a => a -> a) 
         -> LinearLayer i o :> FunLayout (Vector o)
layer f = LinearLayer :> FunLayout f
