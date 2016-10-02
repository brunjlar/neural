{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE BangPatterns #-}

{-|
Module      : Numeric.Neural.Pipes
Description : a pipes API for models
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides a "pipes"-based API for working with models.
-}

module Numeric.Neural.Pipes
    ( TS(..)
    , descentP
    , simpleBatchP
    , cachingBatchP
    , reportTSP
    , consumeTSP
    , qualityP
    , qualityP'
    , classifierAccuracyP
    , classifierAccuracyP'
    , module Pipes
    ) where

import           Data.MyPrelude
import           Numeric.Neural.Model
import           Numeric.Neural.Normalization
import           Data.Utils.Cache
import           Data.Utils.Random            (takeR)
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
        let !eta = f i'
        let (e, m'') = descent m' eta xs
        m'' `deepseq` yield TS
            { tsModel      = m''
            , tsGeneration = i'
            , tsEta        = eta
            , tsBatchError = e
            }
        loop m'' (succ i')

-- | A simple 'Producer' of mini-batches.
simpleBatchP :: MonadRandom m
                => [a]              -- ^ all available samples
                -> Int              -- ^ mini-batch size
                -> Producer [a] m r
simpleBatchP xs n = forever $ lift (takeR n xs) >>= yield

-- | Function 'simpleBatchP' only works when all available samples fit into memory.
--   If this is not the case, 'cachingBatchP' can be used instead.
--   It takes an effectful way to get specific samples and then caches some of those samples
--   in memory for a couple of rounds, drawing mini-batches from the cached values.
--
cachingBatchP :: MonadRandom m
                 => ([Int] -> m [a]) -- ^ get samples with specified indices
                 -> Int              -- ^ number of all available samples
                 -> Int              -- ^ mini-batch size
                 -> Int              -- ^ cache size
                 -> Int              -- ^ number of cache reuses
                 -> Producer [a] m s
cachingBatchP f ns bs cs nr = do
    let c = newCache f cs
    loop c

  where

    loop c = do
       xs <- lift $ takeR cs [0 .. pred ns]
       (ys, c') <- lift $ retrieveC c xs
       replicateM_ nr $ lift (takeR bs ys) >>= yield
       loop c'

-- | A 'Pipe' for progress reporting of model training.
--
reportTSP :: Monad m
             => Int                                    -- ^ report interval
             -> (TS f g a b c -> m ())                 -- ^ report action
             -> Pipe (TS f g a b c) (TS f g a b c) m r
reportTSP n act = P.mapM $ \ts -> do
    when (tsGeneration ts `mod` n == 0) (act ts)
    return ts

-- | A 'Consumer' of training states that decides when training is finished and then returns a value.
--
consumeTSP :: Monad m
              => (TS f g a b c -> m (Maybe x)) -- ^ check whether training is finished and what to return in that case
              -> Consumer (TS f g a b c) m x
consumeTSP check = loop where

    loop = do
        ts <- await
        mx <- lift (check ts)
        case mx of
            Just x  -> return x
            Nothing -> loop

-- | Computes the average "quality" of a given 'Model' over a stream of pairs of input and expected output.
--
qualityP :: (Monad m, Fractional x)
            => Model f g a b c     -- ^ the 'Model'
            -> (b -> c -> c -> x)  -- ^ gives the quality for given input, expected output and actual output
            -> Producer (b, c) m r -- ^ a 'Producer' of pairs of input and expected output
            -> m (Maybe x)         -- ^ the average model quality or 'Nothing' if the 'Producer' was empty.
qualityP m f p = P.fold g (0, 0 :: Int) h $ void p

  where

    g (!xs, !n) (b, c) = let c' = model m b
                             x  = f b c c'
                         in  (x + xs, succ n)

    h (_, 0) = Nothing
    h (x, n) = Just $ x / fromIntegral n

-- | Pure version of 'qualityP'.
--
qualityP' :: Fractional x
             => Model f g a b c    -- ^ the 'Model'
             -> (b -> c -> c -> x) -- ^ gives the quality for given input, expected output and actual output
             -> [(b, c)]           -- ^ list of pairs of input and expected output
             -> Maybe x            -- ^ the average model quality or 'Nothing' if the list was empty.
qualityP' m f xs = runIdentity $ qualityP m f $ each xs

-- | Specialization of 'qualityP' to the case of 'Classifier's.
--
classifierAccuracyP :: (Monad m, Eq c, Fractional x)
                       => Classifier f n b c
                       -> Producer (b, c) m r
                       -> m (Maybe x)
classifierAccuracyP m = qualityP m $ \_ c c' -> if c == c' then 1 else 0

-- | Pure version of 'classifierAccuracyP'.
--
classifierAccuracyP' :: (Eq c, Fractional x)
                        => Classifier f n b c
                        -> [(b, c)]
                        -> Maybe x
classifierAccuracyP' m xs = runIdentity $ classifierAccuracyP m $ each xs
