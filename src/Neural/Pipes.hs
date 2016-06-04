{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Neural.Pipes
Description : a pipes API for models
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This modules provides a "pipes"-based API for working with models.
-}

module Neural.Pipes
    ( TS(..)
    , descentP
    , simpleBatchP
    , reportTSP
    , consumeTSP
    ) where

import           MyPrelude
import           Neural.Model
import           Utils.Random  (takeR)
import           Pipes
import qualified Pipes.Prelude as P

-- | The training state of a model.
--
data TS f g a b c = TS
    { tsModel      :: Model f g a b c -- ^ updated model
    , tsGeneration :: Int             -- ^ generation
    , tsEta        :: Double          -- ^ learning rate
    , tsBatchError :: Double          -- ^ last training error
    }

-- | A 'Pipe' for training a model: It consumes mini-batches of samples from upstream and pushes
--   the updated training state downstream.
--
descentP :: (Foldable h, Monad m) =>
            Model f g a b c                  -- ^ initial model
            -> Int                           -- ^ first generation
            -> (Int -> Double)               -- ^ computes the learning rate from the generation
            -> Pipe (h a) (TS f g a b c) m r
descentP m i f = loop m i where

    loop m' i' = do
        xs <- await
        let eta = f i'
        let (e, m'') = descent m' eta xs
        yield TS
            { tsModel      = m''
            , tsGeneration = i'
            , tsEta        = eta
            , tsBatchError = e
            }
        loop m'' (succ i')

-- | A simple 'Producer' of mini-batches.
simpleBatchP :: MonadRandom m
                => [a]              -- ^ all available samples
                -> Int              -- ^ the mini-batch size
                -> Producer [a] m r
simpleBatchP xs n = forever $ lift (takeR n xs) >>= yield

-- | A 'Pipe' for progress reporting of model training.
--
reportTSP :: Monad m
             => Int                                    -- ^ report interval
             -> (TS f g a b c -> m ())                 -- ^ report action
             -> Pipe (TS f g a b c) (TS f g a b c) m r
reportTSP n act = P.mapM $ \ts -> do
    when (tsGeneration ts `mod` n == 0) (act ts)
    return ts

-- | A 'Consumer' of training states that decides when training is finished and then returns the last training state.
--
consumeTSP :: Monad m
              => (TS f g a b c -> m Bool)                 -- ^ check whether training is finished
              -> Consumer (TS f g a b c) m (TS f g a b c)
consumeTSP check = loop where

    loop = do
        ts <- await
        b <- lift (check ts)
        if b then return ts
             else loop
