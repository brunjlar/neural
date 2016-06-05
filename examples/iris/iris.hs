{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

import           Control.Applicative
import           Control.Arrow        hiding (loop)
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import           MyPrelude
import           Neural
import           Utils

main :: IO ()
main = do
    xs <- readSamples
    printf "read %d samples\n" (length xs)
    (g, q) <- flip evalRandT (mkStdGen 123456) $ do
        m <- modelR irisModel
        runEffect $
                simpleBatchP xs 10
            >-> descentP m 1 (const 0.0001) 
            >-> reportTSP 1000 (report xs)
            >-> consumeTSP (check xs)
    printf "reached prediction accuracy of %5.3f after %d generations\n" q g

  where

    report xs ts = liftIO $ printf "%6d %8.6f %6.4f\n" (tsGeneration ts) (tsBatchError ts) (getQuota xs ts)

    check xs ts = return $
        let g = tsGeneration ts
            q = getQuota xs ts
        in  if g `mod` 1000 == 0 && q >= 0.99
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
    (sqDiff . encode1ofN)
    (\(Attributes sl sw pl pw) -> cons sl (cons sw (cons pl (cons pw nil)))) 
    decode1ofN
