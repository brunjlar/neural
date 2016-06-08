{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Applicative
import           Control.Arrow        hiding (loop)
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           Data.MyPrelude
import           Numeric.Neural
import           Data.Utils

main :: IO ()
main = do
    xs <- readSamples
    printf "read %d samples\n\n" (length xs)
    printf "generation  learning rate  model error  accuracy\n\n"
    (g, q) <- flip evalRandT (mkStdGen 123456) $ do
        m <- modelR (whiten irisModel $ fst <$> xs)
        runEffect $
                simpleBatchP xs 5
            >-> descentP m 1 (\i -> 0.1 * 5000 / (5000 + fromIntegral i))
            >-> reportTSP 1000 (report xs)
            >-> consumeTSP (check xs)
    printf "\nreached prediction accuracy of %5.3f after %d generations\n" q g

  where

    report xs ts = liftIO $ 
        printf "%10d %14.4f %12.6f %9.4f\n" (tsGeneration ts) (tsEta ts) (modelError (tsModel ts) xs) (getQuota xs ts)

    check xs ts = return $
        let g = tsGeneration ts
            q = getQuota xs ts
        in  if g `mod` 100 == 0 && q >= 0.99
            then Just (g, q)
            else Nothing

    getQuota xs ts =
        let ys = map (model $ tsModel ts) $ fst <$> xs :: [Iris]
            n  = length $ filter (uncurry (==)) $ zip ys $ snd <$> xs
            q  = fromIntegral n / fromIntegral (length xs) :: Double
        in  q

data Iris = Setosa | Versicolor | Virginica deriving (Show, Read, Eq, Ord, Enum)

data Attributes = Attributes Double Double Double Double deriving (Show, Read, Eq, Ord)

type Sample = (Attributes, Iris)

sampleParser :: Parser Sample
sampleParser = f <$> (double <* char ',')
                 <*> (double <* char ',')
                 <*> (double <* char ',')
                 <*> (double <* char ',')
                 <*> irisParser
  where 
  
    f sl sw pl pw i = (Attributes sl sw pl pw, i)

    irisParser :: Parser Iris
    irisParser =     string "Iris-setosa"     *> return Setosa
                 <|> string "Iris-versicolor" *> return Versicolor
                 <|> string "Iris-virginica"  *> return Virginica

readSamples :: IO [Sample]
readSamples = do
    ls <- T.lines . T.pack <$> readFile ("examples" </> "iris" </> "data" <.> "csv")
    return $ f <$> ls

  where

    f l = let Right x = parseOnly sampleParser l in x

irisModel :: StdModel (Vector 4) (Vector 3) Attributes Iris
irisModel = mkStdModel
    ((tanhLayer :: Layer 4 2) >>> tanhLayer >>^ softmax)
    crossEntropyError
    (\(Attributes sl sw pl pw) -> cons sl (cons sw (cons pl (cons pw nil)))) 
    decode1ofN
