module Main where

import Nerve

main :: IO ()
main = do
    go (SampleConfig [ 1 .. 20] 100 16) "train"
    go (SampleConfig [21 .. 30] 100 16) "test"
  
  where

    go c f = do
        samples <- mkSamplesIO c
        exportSamples f samples
        printf "exported samples to %s\n\n" f
