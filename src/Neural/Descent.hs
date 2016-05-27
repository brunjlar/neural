{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.Descent
    ( descent
    , batchDescent
    , descentM
    , batchDescentM
    ) where

import MyPrelude
import Numeric.AD
import Neural.Component
import Neural.Layout
import Neural.Sample

descent :: ( Sample a
           , Source (Model a) ~ f
           , Target (Model a) ~ g
           ) => Component f g -> Double -> a -> (Double, Component f g)
descent (Component l ws) eta s = (err, Component l ws') where

    (err, ws') = gradWith'
                     (\x dx -> x - eta * dx)
                     (\ws'' -> modelError s $ compute l ws'' $ auto <$> toSource s)
                     ws

batchDescent :: ( Sample a
                , Source (Model a) ~ f
                , Target (Model a) ~ g
                ) => Component f g -> Double -> [a] -> (Double, Component f g)
batchDescent c eta xs = let ic         = iterComponent c
                            (err, ic') = descent ic eta (Batch xs)
                            c'         = c & _weights .~ (ic' ^. _weights)
                        in  (err, c')

descentM :: ( Sample a
            , Source (Model a) ~ f
            , Target (Model a) ~ g
            , MonadState (Component f g) m
            ) => Double -> a -> m Double
descentM eta s = do
    c <- get
    let (err, c') = descent c eta s
    put c'
    return err

batchDescentM :: ( Sample a
                 , Source (Model a) ~ f
                 , Target (Model a) ~ g
                 , MonadState (Component f g) m
                 ) => Double -> [a] -> m Double
batchDescentM eta xs = do
    c <- get
    let (err, c') = batchDescent c eta xs
    put c'
    return err
