{-# OPTIONS_HADDOCK show-extensions #-}

module Numeric.Neural.Reinforcement
    ( reinforcementBatchP
    , module Reinforcement
    ) where

import           Data.MyPrelude
import           Numeric.Neural.Model
import           Numeric.Neural.Normalization
import           Numeric.Nueral.Pipes
import           Data.Utils.Cache
import           Data.Utils.Random            (takeR)
import           Pipes
import qualified Pipes.Prelude as P



NN :: Model _ _ _ CarState (Prob DriveState)

mkStochasitcModel _ _ _ b c =
  mkStdModel _ _ (b,c) b (mkProb c)

class Discrete a where
  discretize :: a -> a

instance Num a => Discretize a where
  discretize val =     

class ProbWrapper p where
  sample (p val) = random 
  liftProb action =
  mkProb ? = ? :: (* -> *)

instance Num ProbWrapper where
  (*) = 

-- | Reinforcement learning needs a new example set for each iteration
--   This takes an effectful way to run an episode with the current model
--   and collect the actions from said episode and their resulting 'advantages'
--   NB this requires a stochastic policy model and
--   uses the 'advantage' to scale the probabilities for training
--
reinforcementBatchP :: MonadRandom m, ProbWrapper p
                 => (Model f g (b,p c) b (p c) -> IO [(b,c,Double)]) -- ^ get input,action,advantage samples based on current model
                 -> Int                                              -- ^ mini-batch size
                 -> Producer (TS f g (b,p c) b (p c)) [(b,p c)] m s  -- need the current model state to generate new samples, but we do not change the model
reinforcementBatchP f ns = do
    loop
  where

    loop c = do
       ts <- await
       xs <- take ns $ f (model ts) --TODO how to select 'ns' samples
       map (\(i,o,advtg) -> (i,(liftProb o)*advtg)) xs >>= yield
       loop c'

