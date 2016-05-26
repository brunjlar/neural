{-# LANGUAGE GADTs #-}

module Neural.Descent
    ( descent
    ) where

import MyPrelude
import Numeric.AD
import Neural.Components
import Neural.Layouts

descent :: Functor f => Component f Identity -> Double -> f Double -> Component f Identity
descent (Component l ws) eta xs = Component l ws' where

    ws' = gradWith
            (\x dx -> x - eta * dx)
            (\ws'' -> runIdentity $ compute l ws'' $ auto <$> xs)
            ws
