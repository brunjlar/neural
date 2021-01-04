{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE BangPatterns #-}

module Numeric.Neural.Reinforcement
    ( reinforcementBatchP
    , reinforceDescentP
    , Scalable(..)
    ) where

import           Data.MyPrelude
import           Numeric.Neural.Model
import           Numeric.Neural.Normalization
import           Numeric.Neural.Pipes
import           Data.Utils.Cache
import           Data.Utils.Random            (takeR)
import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude as P



class Scalable a where
  scale :: a -> Double -> a
instance Scalable Double where
  scale = (*)
--instance Scalable DriveStateProbs where
--  scale ds{..} s = ds{..} -- * s

-- | Reinforcement learning needs a new example set for each iteration
--   This takes an effectful way to run an episode with the current model
--   and collect the actions (as 100% probs) from said episode and their resulting 'advantages'
--   NB this requires a stochastic policy model and
--   automatically uses the 'advantage' to scale the probabilities for training
--
reinforcementBatchP :: (Scalable c)
                       -- TODO this way Prob is built into the model and IO, would rather not have Prob built into IO op
                       => ((Model f g (b,c) b c) -> IO [(b,c,Double)]) -- ^ get input,action(where action is a 100% prob on taken action),advantage samples based on current model
                       -> Int                                         -- ^ mini-batch size
                       -> (Model f g (b,c) b c)
                       -> Server (Model f g (b,c) b c) [(b,c)] IO ()      -- need the current model state to generate new samples, but we do not change the model
reinforcementBatchP f ns ts =  forever $ do
  xs <- lift $ f $ ts --TODO how to select 'ns' samples
  let xs' = map (\(i,o,advtg) -> (i,scale o advtg)) xs 
  respond xs'


-- | Training a reinforcement model: 
--   Pushes the current model upsteam
--   Consumes the response of samples (already scaled by advantage) from upstream simulator
--   and sends the updated training state downstream.
--
reinforceDescentP :: (Show a,Foldable h, Monad m) =>
            Model f g a b c                  -- ^ initial model
            -> Int                           -- ^ first generation
            -> (Int -> Double)               -- ^ computes the learning rate from the generation
            -> Proxy (Model f g a b c) (h a) () (TS f g a b c) m r
reinforceDescentP m i f = loop m i where

    loop m' i' = do
        xs <- request m'
        let !eta = f i'
        let (e, m'') = descent m' eta xs
        m'' `deepseq` yield TS
            { tsModel      = m''
            , tsGeneration = i'
            , tsEta        = eta
            , tsBatchError = e
            }
        loop m'' (succ i')

