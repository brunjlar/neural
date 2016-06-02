{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Neural.DescentSpec (spec) where

import Control.Arrow         hiding (loop)
import Control.Monad.Random
import MyPrelude
import Neural
import Test.Hspec
import Utils

spec :: Spec
spec = describe "descent" $

    it "should approximate the square roots in [0, 4]" $
        go >>= (`shouldSatisfy` (< 0.015))

sqrtModel :: Model Double Analytic Double Double Double
sqrtModel = Model
    { component = c
    , err       = e
    , finalize  = (>>^ fromAnalytic >>^ fromJust)
    }

  where

    c :: Component Double Analytic
    c = let l1     = tanhLayer :: Layer 1 2
            l2     = linearLayer :: Layer 2 1
            toIn x = cons (fromDouble x) nil
        in  toIn ^>> l1 >>> l2 >>^ vhead

    e :: Err Double Analytic Double
    e c' = proc x -> do
      y <- c' -< x
      let d = y - fromDouble (sqrt x)
      returnA -< d * d

samples :: [Double]
samples = [0, 0.001 .. 4]

eta :: Double
eta = 0.03

go :: IO Double
go = evalModelM sqrtModel (mkStdGen 691245) $ do 
   
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
    
    return e

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
