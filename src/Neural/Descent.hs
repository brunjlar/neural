{-# LANGUAGE RankNTypes #-}

module Neural.Descent
    ( Err
    , descent
    , descentM
    ) where

import MyPrelude
import Neural.Component
import Neural.Monad
import Utils.Analytic
import Utils.Statistics (mean)

type Err a b c = forall t. Component' t a b -> Component' t c Analytic

descent :: Component a b -> Err a b c -> Double -> [c] -> (Double, Component a b)
descent (Component ws c i) err eta xs = (e, Component ws' c i) where

    (e, ws') = gradient (\w dw -> w - eta * dw) f ws

    f zs = mean [runC (err c) x zs | x <- xs]

descentM :: Monad m => Err a b c -> Double -> [c] -> ComponentM a b m Double
descentM err eta xs = do
    c <- get
    let (e, c') = descent c err eta xs
    put c'
    return e
