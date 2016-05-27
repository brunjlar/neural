{-# LANGUAGE RankNTypes #-}

module Neural.Analytic
    (
    ) where

import Control.Category
import MyPrelude
import Numeric.AD
import Prelude          hiding (id, (.))

newtype Analytic f g = Analytic (forall a. RealFloat a => f a -> g a)

instance Category Analytic where

    id = Analytic id

    Analytic f . Analytic g = Analytic (f . g)

gradient :: (RealFloat a, Traversable t) => (a -> a -> b) -> Analytic t Identity -> t a -> (a, t b)
gradient g f xs = gradWith' g  (h f) xs where

    h (Analytic f') = runIdentity . f'
