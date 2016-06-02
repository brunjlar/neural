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
    ) where

import MyPrelude
import Neural.Model
import Utils.Analytic

-- | The @'ModelM' a b c d e m@ monad simplifies working with models of type @'Model' a b c d e@.
--
newtype ModelM a b c d e m x = ModelM (StateT (Model a b c d e) (RandT StdGen m) x)
    deriving (Functor, Applicative, Monad, MonadState (Model a b c d e), MonadRandom, MonadIO)

-- | Runs a computation in the @'ModelM' a b c d e m@ monad.
--
runModelM :: Monad m 
             => Model a b c d e                -- ^ initial model
             -> StdGen                         -- ^ initial random number generator
             -> ModelM a b c d e m x           -- ^ the monadic expression to run
             -> m (x, Model a b c d e, StdGen) -- ^ returns result, new model and new random number generator
runModelM m g (ModelM y) = 
    runRandT (runStateT y m) g >>= \((x, m'), g') -> 
    return (x, m', g')

-- | Evaluates a computation in the @'ModelM' a b c d e m@ monad.
--
evalModelM :: Monad m 
              => Model a b c d e      -- ^ initial model
              -> StdGen               -- ^ initial random number generator
              -> ModelM a b c d e m x -- ^ the monadic expression to evaluate
              -> m x                  -- ^ returns the result
evalModelM m g y = runModelM m g y >>= \(x, _, _) -> return x

-- | Monadic version of 'model'.
--
modelM :: Monad m => d -> ModelM a b c d e m e
modelM d = get >>= \m -> return $ model m d

-- | Monadic version of 'errorModel'.
--
errorM :: (Foldable f, Monad m) => f c -> ModelM a b c d e m Analytic
errorM xs = get >>= \m -> return $ modelError m xs

-- | Monadic version of 'modelR'.
--
randomizeM :: Monad m => ModelM a b c d e m ()
randomizeM = do
    m  <- get
    m' <- modelR m
    put m'
