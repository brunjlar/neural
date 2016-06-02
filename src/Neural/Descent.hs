{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}

{-|
Module      : Natural.Descent
Description : gradient descent
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides /gradient descent/ for components. 
A special case of this is the /backpropagation algorithm/ for neural networks.
-}

module Neural.Descent
    ( Err
    , getError
    , descent'
    , descent
    , descentM
    ) where

import Control.Arrow
import MyPrelude
import Neural.Component
import Neural.Monad
import Utils.Analytic
import Utils.Arrow
import Utils.Statistics (mean)

-- | An element @err@ of @'Err' a b c@ transforms a component @c@ into a component @err c@
--   (with the same weights) that measures @c@'s error.
--   Then gradient descent can be used to minimize @c@'s error with respect to @err@.
--
type Err a b c = forall t. Component' t a b -> Component' t c Analytic

-- | Computes the average error of a component for an error transformation and collection of samples.
--
getError :: (Functor f, Foldable f) => Component a b -> Err a b c -> f c -> Analytic
getError (Component ws c _) err xs = let c' = convolve (err c) >>^ toList >>^ mean
                                     in  runC c' xs $ fromDouble <$> ws

-- | This function performs one step of gradient descent for the given component and error transformation.
--
descent' :: Component a b              -- ^ the component whose error should be decreased 
            -> Err a b c               -- ^ the error transformation
            -> Double                  -- ^ the learning rate
            -> c                       -- ^ a sample
            -> (Double, Component a b) -- ^ the sample error and the improved component
descent' (Component ws c i) err eta x = (e, Component ws' c i) where

    (e, ws') = gradient (\w dw -> w - eta * dw) (runC (err c) x) ws

-- | This function performs one step of gradient descent for a 'mini batch' of samples
--   (by taking the mean over all errors for samples in the batch).
descent :: Component a b              -- ^ the component whose error should be decreased 
           -> Err a b c               -- ^ the error transformation
           -> Double                  -- ^ the learning rate
           -> [c]                     -- ^ the mini batch of samples
           -> (Double, Component a b) -- ^ the mean error and the improved component
descent c err = descent' c err' where

    err' c' = convolve (err c') >>^ mean

-- | This is the monadic version of 'descent': It performs one step of gradient descent on a 'mini batch'
--   of samples and implicitly updates the state-component's weights.
--
descentM :: Monad m 
            => Err a b c               -- ^ the error transformation
            -> Double                  -- ^ the learning rate
            -> [c]                     -- ^ the mini batch of samples
            -> ComponentM a b m Double -- ^ the mean error 
descentM err eta xs = do
    c <- get
    let (e, c') = descent c err eta xs
    put c'
    return e
