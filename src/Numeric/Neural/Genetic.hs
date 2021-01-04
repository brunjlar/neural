{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.Neural.Genetic
    ( geneticTrain ,
      geneticTrainL
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
import Data.List

-- | Just try random weights and always take the best one
--
geneticTrain :: 
       ((Model f g a b c) -> IO Double) -- ^ run the model and get cost
       -> Int                            -- ^ generation size
       -> Int                            -- ^ number of spawn based on previous best
       -> (Double,(Model f g a b c))
       -> Producer (TS f g a b c) IO ()      -- produces better and better models (hopefully)
geneticTrain f gs ss ts = loop ts 
   where
    loop oldBest = do
       randomGen <- mapM (lift. evalRandIO. modelR) (replicate (gs-ss) (snd oldBest))
       improvedGen <- mapM (lift. evalRandIO. modelG) (replicate ss (snd oldBest))
       let newGen = randomGen++improvedGen
       costs <- mapM (lift. f) newGen
       let cs = zip costs newGen
       let bestModel = maximumBy (\x y -> compare (fst x) (fst y)) (oldBest:cs)
       bestModel `deepseq` yield TS
            { tsModel      = snd bestModel
            , tsGeneration = 1 --not sure this is used anyway
            , tsEta        = 0 --also dont think this matters
            , tsBatchError = fst bestModel
            }
       loop $ bestModel
    

type ModelSet f g a b c f' g' a' b' c' = ((Model f g a b c), (Model f' g' a' b' c'))

-- | Just try random weights and always take the best one
--
geneticTrainL ::
       (ModelSet f g a b c f' g' a' b' c' -> IO Double) -- ^ run the model and get cost
       -> Int                            -- ^ generation size
       -> Int
       -> (Double,ModelSet f g a b c f' g' a' b' c')
       -> Producer (TS f g a b c) IO ()      -- produces better and better models (hopefully)
geneticTrainL f gs ss ts = loop ts 
   where
    loop oldBestL = do
       randomGen <- mapM (lift. (\ms-> (evalRandIO$ modelR$ fst ms) >>= (\x-> return (x,snd ms)))) (replicate (gs-ss) (snd oldBestL))
       randomGen2 <- mapM (lift. (\ms-> (evalRandIO$ modelR$ snd ms) >>= (\x-> return (fst ms,x)))) (replicate (gs-ss) (snd oldBestL))
       improvedGen <- mapM (lift. (\ms-> (evalRandIO$ modelG$ fst ms) >>= (\x-> return (x,snd ms)))) (replicate (ss) (snd oldBestL))
       improvedGen2 <- mapM (lift. (\ms-> (evalRandIO$ modelG$ snd ms) >>= (\x-> return (fst ms, x)))) (replicate (ss) (snd oldBestL))
       let newGen = randomGen++improvedGen++randomGen2++improvedGen2
       costs <- mapM (lift. f) newGen
       let cs = zip costs newGen
       let bestModelL = maximumBy (\x y -> compare (fst x) (fst y)) (oldBestL:cs)
       bestModelL `deepseq` yield TS
            { tsModel      = fst $ snd bestModelL
            , tsGeneration = 1 --not sure this is used anyway
            , tsEta        = 0 --also dont think this matters
            , tsBatchError = fst bestModelL
            }
       loop $ bestModelL
    
