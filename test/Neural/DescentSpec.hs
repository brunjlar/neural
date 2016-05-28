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

l :: Layer 1 1
l = let l1 = layer tanh  :: LAYOUT (Vector 1) (Vector 2)
        l2 = linearLayer :: LAYOUT (Vector 2) (Vector 1)
    in  l2 . l1

samples :: [(Vector 1 Double, Vector 1 Double)]
samples = [(cons (x * x) nil, cons x nil) | x <- [0, 0.001 .. 2]]

eta :: Double
eta = 0.1

go :: IO Double
go = evalInitModelM l stdModel (mkStdGen 691245) $ do 

    e0 <- err
    liftIO $ putStrLn $ "initial error is " ++ show e0
    nl

    (i, e) <- loop (1 :: Int)
    nl
    liftIO $ printf "%d steps, error: %f\n" i e
    nl

    forM_ [0 :: Double, 0.1 .. 4] $ \x -> do
        y' <- act x
        let y  = sqrt x
            e' = abs (y - y')
        liftIO $ printf "%3.1f %10.8f %10.8f %10.8f\n" x y y' e'
    
    return e

  where

    nl = liftIO $ putStrLn ""

    act x = do
        ys <- activateM (cons x nil, cons (sqrt x) nil)
        return $ head $ toList ys

    err' x = act x >>= \y -> return $ abs (sqrt x - y)

    err = mean <$> mapM err' [0, 0.1 .. 4]

    step i = do
        xs <- takeR 20 samples
        e  <- batchDescentM eta xs
        e' <- err
        when (i `mod` 100 == 0) $ liftIO $ printf "%6d %10.8f %10.8f\n" i e e'
        return e'

    loop i = do
        e' <- step i
        if (e' < 0.015 || i == 10000) then return (i, e')
                                      else loop (succ i)
