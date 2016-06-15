{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Category
import Criterion.Main
import Data.MyPrelude
import Data.Utils
import Data.Void
import Numeric.Neural
import Prelude           hiding (id, (.))

main :: IO ()
main = defaultMain 
    [ bgroup "white" 
        [ bench "10/200"     $ whnf (w   10)   200
        , bench "10/2000"    $ whnf (w   10)  2000
        , bench "10/20000"   $ whnf (w   10) 20000
        , bench "100/200"    $ whnf (w  100)   200
        , bench "100/2000"   $ whnf (w  100)  2000
        , bench "100/20000"  $ whnf (w  100) 20000
        , bench "1000/200"   $ whnf (w 1000)   200
        , bench "1000/2000"  $ whnf (w 1000)  2000
        , bench "1000/20000" $ whnf (w 1000) 20000
        ]
    , env setupEnv $ \ ~(m, xss) -> bgroup "linear"
        [ l m xss  1 5 
        , l m xss  5 5
        , l m xss 10 5
        ]
    ]

w :: Int -> Int -> Double
w sampleCount testCount = flip evalRand (mkStdGen 123456) $ do
    stats   <- mkStats'
    samples <- replicateM sampleCount $ mkSample stats
    let m = whiten model' samples
    xss <- replicateM testCount $ mkSample stats
    return $ sum [model m xs | xs <- xss]

  where

    mkStats' :: MonadRandom m => m (Vector Width (Double, Double))
    mkStats' = sequenceA (pure $ (,) <$> getRandomR (-100, 100) <*> getRandomR (0.1, 20))

    mkSample :: MonadRandom m => Vector Width (Double, Double) -> m (Vector Width Double)
    mkSample = mapM $ uncurry boxMuller'

    model' :: Model (Vector Width) Identity Void (Vector Width Double) Double
    model' = Model (cArr $ Diff $ Identity . sum) absurd id runIdentity

type Width = 10

l :: M -> [Vector Width' Double] -> Int -> Int -> Benchmark
l m xss batchSize steps = bench (printf "%d/%d" batchSize steps) $ whnf l' steps where

    l' :: Int -> Double
    l' steps' = 
        let m' = loop steps' m 
            xs = pure 0
        in  modelError m' [(xs, xs)]

    loop :: Int -> M -> M
    loop 0  m' = m'
    loop !n m' =
        let m'' = m' `deepseq` snd $ descent m' 0.01 [(xs, xs) | xs <- take batchSize xss]
        in  loop (pred n) m''

setupEnv :: IO (M, [Vector Width' Double])
setupEnv = return $ flip evalRand (mkStdGen 987654) $ do
    let e xs = Diff $ Identity . sqDiff (fromDouble <$> xs)
    m   <- modelR $ mkStdModel linearLayer e id id
    xss <- replicateM 100 $ let r = getRandomR (-5, 5) in sequence $ pure r
    return (m, xss)

type M = StdModel (Vector Width') (Vector Width') (Vector Width' Double) (Vector Width' Double)

type Width' = 100
