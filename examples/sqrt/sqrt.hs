{-# LANGUAGE DataKinds #-}

import Control.Arrow        hiding (loop)
import Control.Monad.Random
import MyPrelude
import Neural
import Utils

main :: IO ()
main = evalModelM sqrtModel (mkStdGen 691245) $ do 
   
    randomizeM

    e0 <- getErr
    liftIO $ putStrLn $ "initial error is " ++ show e0
    nl

    (i, e) <- loop (1 :: Int)
    nl
    liftIO $ printf "%d steps, error: %f\n" i e
    nl

    forM_ [0 :: Double, 0.1 .. 4] $ \x -> do
        y' <- modelM x
        let y  = sqrt x
            e' = abs (y - y')
        liftIO $ printf "%3.1f %10.8f %10.8f %10.8f\n" x y y' e'

  where

    nl = liftIO $ putStrLn ""

    getErr' x = modelM x >>= \y -> return $ abs (sqrt x - y)

    getErr = mean <$> mapM getErr' [0, 0.1 .. 4]

    step i = do
        xs <- takeR 10 samples
        e  <- descentM eta xs
        e' <- getErr
        when (i `mod` 100 == 0) $ liftIO $ printf "%6d %10.8f %10.8f\n" i e e'
        return e'

    loop i = do
        e' <- step i
        if e' < 0.015 || i == 10000 then return (i, e')
                                    else loop (succ i)

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

samples :: [(Double, Double)]
samples = [(x, sqrt x) | x <- [0, 0.001 .. 4]]

eta :: Double
eta = 0.03

