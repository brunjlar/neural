{-# LANGUAGE DataKinds #-}

import           Control.Category
import           Control.Monad.Random
import           Data.MyPrelude
import           Data.Utils
import           Graph
import           Numeric.Neural
import qualified System.Console.ANSI  as ANSI
import           Prelude              hiding ((.))

main :: IO ()
main = flip evalRandT (mkStdGen 739570) $ do
    let xs = [0, 0.01 .. 2 * pi]
    m <- modelR $ whiten sinModel xs
    runEffect $
            simpleBatchP [(x, sin x) | x <- xs] 10
        >-> descentP m 1 (const 0.5)
        >-> reportTSP 50 report
        >-> consumeTSP check

  where

    sinModel :: StdModel (Vector 1) (Vector 1) Double Double
    sinModel = mkStdModel
        (tanhLayer . (tanhLayer :: Layer 1 4))
        (\x -> Diff $ Identity . sqDiff (pure $ fromDouble x))
        pure
        vhead

    getError ts =
        let m = tsModel ts
        in  maximum [abs (sin x - model m x) | x <- [0, 0.1 .. 2 * pi]]

    report ts = liftIO $ do
        ANSI.clearScreen
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
        ANSI.setCursorPosition 0 0
        printf "Generation %d\n" (tsGeneration ts)
        ANSI.setSGR [ANSI.Reset]
        graph (model (tsModel ts)) 0 (2 * pi) 20 50

    check ts = return $ if getError ts < 0.1 then Just () else Nothing
