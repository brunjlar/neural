{-# LANGUAGE RankNTypes #-}

module Neural.Analytic
    ( Analytic(..)
    , fmapAnalytic
    ) where

import Control.Category
import Prelude          hiding (id, (.))

newtype Analytic f g = Analytic (forall a. RealFloat a => f a -> g a)

instance Category Analytic where

    id = Analytic id

    Analytic f . Analytic g = Analytic (f . g)

fmapAnalytic :: Functor f => (forall a. RealFloat a => a -> a) -> Analytic f f
fmapAnalytic g = Analytic $ fmap g
