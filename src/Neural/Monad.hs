{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Neural.Monad
Description : a monad for working with models
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the 'ModelM' monad for working with models. 
It is basically a state monad (which holds the model in its state)
with the added functionality of random number generation.
-}

module Neural.Monad
    ( ModelM
    , runModelM
    , evalModelM
    , modelM
    , errorM
    , randomizeM
    , descentM
    ) where

import MyPrelude
import Neural.Model

-- | The @'ModelM' f g a b c m@ monad simplifies working with models of type @'Model' f g a b c@.
--
newtype ModelM f g a b c m x = ModelM (StateT (Model f g a b c) (RandT StdGen m) x)
    deriving (Functor, Applicative, Monad, MonadState (Model f g a b c), MonadRandom, MonadIO)

-- | Runs a computation in the @'ModelM' f g a b c m@ monad.
--
runModelM :: Monad m 
             => Model f g a b c                -- ^ initial model
             -> StdGen                         -- ^ initial random number generator
             -> ModelM f g a b c m x           -- ^ the monadic expression to run
             -> m (x, Model f g a b c, StdGen) -- ^ returns result, new model and new random number generator
runModelM m g (ModelM y) = 
    runRandT (runStateT y m) g >>= \((x, m'), g') -> 
    return (x, m', g')

-- | Evaluates a computation in the @'ModelM' a b c d e m@ monad.
--
evalModelM :: Monad m 
              => Model f g a b c      -- ^ initial model
              -> StdGen               -- ^ initial random number generator
              -> ModelM f g a b c m x -- ^ the monadic expression to evaluate
              -> m x                  -- ^ returns the result
evalModelM m g y = runModelM m g y >>= \(x, _, _) -> return x

-- | Monadic version of 'model'.
--
modelM :: Monad m => b -> ModelM f g a b c m c
modelM b = get >>= \m -> return $ model m b

-- | Monadic version of 'errorModel'.
--
errorM :: (Foldable h, Monad m) => h a -> ModelM f g a b c m Double
errorM xs = get >>= \m -> return $ modelError m xs

-- | Monadic version of 'modelR'.
--
randomizeM :: Monad m => ModelM f g a b c m ()
randomizeM = do
    m  <- get
    m' <- modelR m
    put m'

-- | This is the monadic version of 'descent': It performs one step of gradient descent on a "mini batch"
--   of samples and returns the average sample error.
--
descentM :: (Foldable h, Monad m)
            => Double                    -- ^ the learning rate
            -> h a                       -- ^ the mini batch of samples
            -> ModelM f g a b c m Double -- ^ returns the average error 
descentM eta xs = do
    m <- get
    let (e, m') = descent m eta xs
    put m'
    return e
