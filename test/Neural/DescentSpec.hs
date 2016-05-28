{-# LANGUAGE DataKinds #-}

module Neural.DescentSpec (spec) where

import Control.Category
import Control.Monad.Random
import Neural
import Prelude               hiding (id, (.))
import Test.Hspec

spec :: Spec
spec = do
    addSpec

addSpec :: Spec
addSpec = describe "batchDescent" $

    it "should approximate the square roots in [0, 4]" $
        go >>= (`shouldSatisfy` (< 0.015))

l :: LAYOUT (Vector 1) (Vector 1)
l = let l1 = layer tanh  :: LAYOUT (Vector 1) (Vector 2)
        l2 = linearLayer :: LAYOUT (Vector 2) (Vector 1)
    in  l2 . l1

samples :: [(Vector 1 Double, Vector 1 Double)]
samples = [(cons (x * x) nil, cons x nil) | x <- [0, 0.001 .. 2]]

eta :: Double
eta = 0.1

go :: IO Double
go = flip evalRandT (mkStdGen 691245) $ do

    c <- componentR l
    liftIO $ putStrLn $ "initialized net, weights are " ++ show (c ^. _weights)
    nl

    let m      = stdModel c
    liftIO $ putStrLn $ "initial error is " ++ show (err m)
    nl

    (i, e, m') <- loop (1 :: Int) m
    nl
    liftIO $ printf "%d steps, error: %f\n" i e
    nl

    forM_ [0 :: Double, 0.1 .. 4] $ \x -> liftIO $ do
       let y  = sqrt x
           y' = act m' x
           e' = abs (y - y')
       printf "%3.1f %10.8f %10.8f %10.8f\n" x y y' e'
    
    return e

  where

    nl = liftIO $ putStrLn ""

    act m x = head $ toList $ activateModel m (cons x nil, cons (sqrt x) nil)

    err' m x = abs (sqrt x - act m x)

    err m = mean [err' m x | x <- [0, 0.1 .. 4]] 

    step i m = do
        xs <- takeR 20 samples
        let (e, m') = batchDescent m eta xs
            e'      = err m
        when (i `mod` 100 == 0) $ liftIO $ printf "%6d %10.8f %10.8f\n" i e e'
        return (e', m')

    loop i m = do
        (e', m') <- step i m
        if (e' < 0.015 || i == 10000) then return (i, e', m')
                                      else loop (succ i) m'
