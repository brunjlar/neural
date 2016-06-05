{-# LANGUAGE DataKinds #-}

import Control.Arrow        hiding (loop)
import Control.Monad.Random
import MyPrelude
import Neural
import Utils

main :: IO ()
main = do
    m <- flip evalRandT (mkStdGen 691245) $ do
        m <- modelR sqrtModel
        runEffect $
                simpleBatchP [(x, sqrt x) | x <- [0, 0.001 .. 4]] 10
            >-> descentP m 1 (const 0.03) 
            >-> reportTSP 100 report
            >-> consumeTSP check
    
    forM_ [0 :: Double, 0.1 .. 4] $ \x -> do
        let y' = model m x
            y  = sqrt x
            e = abs (y - y')
        printf "%3.1f %10.8f %10.8f %10.8f\n" x y y' e

  where

    sqrtModel :: StdModel (Vector 1) (Vector 1) Double Double
    sqrtModel = mkStdModel c e pure vhead

      where

        c :: Layer 1 1
        c = let l1 = tanhLayer   :: Layer 1 2
                l2 = linearLayer :: Layer 2 1
            in  l1 >>> l2

        e :: Double -> Vector 1 Analytic -> Analytic
        e y y' = let d = (-) <$> y' <*> pure (fromDouble y)
                 in  d <%> d

    getErr ts = let m = tsModel ts in mean [abs (sqrt x - model m x) | x <- [0, 0.1 .. 4]]

    report ts = do
        let e = getErr ts
        liftIO $ printf "%6d %10.8f %10.8f\n" (tsGeneration ts) (tsBatchError ts) e

    check ts = do
        let e = getErr ts
        if e < 0.015 then do
            liftIO $ printf "\nmodel error after %d generations: %f\n\n" (tsGeneration ts)  e
            return $ Just (tsModel ts)
                     else return Nothing
