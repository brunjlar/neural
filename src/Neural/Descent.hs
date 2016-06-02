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

This module provides /gradient descent/ for models. 
A special case of this is the /backpropagation algorithm/ for neural networks.
-}

module Neural.Descent
    ( descent
    , descentM
    ) where

import Control.Arrow
import MyPrelude
import Neural.Model
import Neural.Monad
import Utils.Analytic
import Utils.Arrow
import Utils.Statistics (mean)

descent' :: Traversable t => ParamFun t a Analytic
            -> Double            
            -> a                  
            -> t Double
            -> (Double, t Double)
descent' c eta x = gradient (\w dw -> w - eta * dw) (runPF c x)

-- | This function performs one step of gradient descent for a "mini batch" of samples
--   (by taking the average of all errors for samples in the batch).
--
descent :: (Functor f, Foldable f)
           => Model a b c d e           -- ^ the model whose error should be decreased 
           -> Double                    -- ^ the learning rate
           -> f c                       -- ^ a mini batch of samples
           -> (Double, Model a b c d e) -- ^ returns the average sample error and the improved model
descent m eta xs = case component m of
    Component ws c i ->
        let c'       = err m c
            c''      = convolve c' >>^ toList >>^ mean
            (e, ws') = descent' c'' eta xs ws
        in  (e, m { component = Component ws' c i })

-- | This is the monadic version of 'descent': It performs one step of gradient descent on a "mini batch"
--   of samples and returns the average sample error.
--
descentM :: (Functor f, Foldable f, Monad m)
            => Double                        -- ^ the learning rate
            -> f c                           -- ^ the mini batch of samples
            -> ModelM a b c d e m Double     -- ^ returns the average error 
descentM eta cs = do
    m <- get
    let (e, m') = descent m eta cs
    put m'
    return e
