{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import           Control.Monad
import           Control.Monad.ST   (ST, runST)
import qualified Data.Array.Unboxed as U
import qualified Data.Array.ST      as ST

graph :: (Double -> Double) -> Double -> Double -> Int -> Int -> IO ()
graph f xMin xMax height width = do
    let a = array
    forM_ [1..height] $ \y -> do
        forM_ [1..width] $ \x -> putChar (a U.! (y, x))
        putChar '\n'

  where

    array :: U.UArray (Int, Int) Char
    array = runST $ do
        a <- ST.newArray ((1, 1), (height, width)) ' ' :: forall s. ST s (ST.STUArray s (Int, Int) Char)
        let dx       = (xMax - xMin) / fromIntegral (pred width)
            xys      = [let x = xMin + fromIntegral j * dx in (x, f x) | j <- [0 .. pred width]]
            ys       = snd <$> xys
            yMin     = minimum ys
            yMax     = maximum ys
            x_to_j x = round $ (x - xMin) / (xMax - xMin) * fromIntegral (pred width) + 1
            j_to_x j = xMin + dx * fromIntegral (pred j)
            y_to_i y = height - round ((y - yMin) / (yMax - yMin) * fromIntegral (pred height))

        draw a x_to_j j_to_x y_to_i
        ST.freeze a

      where

        draw :: forall s. ST.STUArray s (Int, Int) Char -> (Double -> Int) -> (Int -> Double) -> (Double -> Int) -> ST s ()
        draw a x_to_j j_to_x y_to_i = drawXAxis >> drawYAxis >> drawGraph

          where

            drawXAxis :: ST s ()
            drawXAxis = do
                let i = y_to_i 0
                when (i >= 1 && i <= height) $
                    forM_ [1..width] $ \j -> ST.writeArray a (i, j) '-'

            drawYAxis :: ST s ()
            drawYAxis = do
                let j = x_to_j 0
                when (j >= 1 && j <= width) $
                    forM_ [1..height] $ \i -> ST.writeArray a (i, j) '-'

            drawGraph :: ST s ()
            drawGraph = forM_ [1..width] $ \j -> do
                let x = j_to_x j
                    y = f x
                    i = y_to_i y
                ST.writeArray a (i, j) '*'
