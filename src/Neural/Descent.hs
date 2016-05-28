{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.Descent
    ( descent
    , batchDescent
    , descentM
    , batchDescentM
    , getModelError
    , getModelErrorM
    ) where

import MyPrelude
import Numeric.AD
import Neural.Component
import Neural.Layout
import Neural.Model
import Neural.Monad

descent :: Functor f => Model f g a b -> Double -> a -> (Double, Model f g a b)
descent m eta s = case component m of
    Component l ws -> (err, m { component = Component l ws' }) where

        (err, ws') = gradWith'
                         (\x dx -> x - eta * dx)
                         (\ws'' -> modelError m auto s $ compute l ws'' $ auto <$> toInput m s)
                         ws

batchDescent :: Functor f => Model f g a b -> Double -> [a] -> (Double, Model f g a b)
batchDescent m eta xs = let im         = batch m
                            (err, im') = descent im eta xs
                            m'         = m & _component . _weights .~ (im' ^. _component . _weights)
                        in  (err, m')

descentM :: (Functor f, Monad m) => Double -> a -> ModelM f g a b m Double 
descentM eta s = do
    m <- get
    let (err, m') = descent m eta s
    put m'
    return err

batchDescentM :: (Functor f, Monad m) => Double -> [a] -> ModelM f g a b m Double
batchDescentM eta xs = do
    m <- get
    let (err, m') = batchDescent m eta xs
    put m'
    return err

getModelError :: Functor f => Model f g a b -> a -> Double
getModelError m s = let (err, _) = descent m 1 s in err

getModelErrorM :: (Functor f, Monad m) => a -> ModelM f g a b m Double
getModelErrorM s = do
    m <- get
    return $ getModelError m s
