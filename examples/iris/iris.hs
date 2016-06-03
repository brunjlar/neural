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
    evalModelM irisModel (mkStdGen 123456) $ do

        let getError' = errorM xs

        let getQuota = do
                ys <- mapM modelM $ fst <$> xs
                let n = length $ filter (uncurry (==)) $ zip ys $ snd <$> xs
                    q = fromIntegral n / fromIntegral (length xs) :: Double
                return q

        let loop i = do
                batch <- takeR batchSize xs
                _     <- descentM eta batch
                if i `mod` 1000 == 0 
                    then do
                        e <- getError'
                        q <- getQuota
                        liftIO $ printf "%6d %8.6f %6.4f\n" i e q
                        unless (q >= 0.99) $ loop (succ i)
                    else loop (succ i)

        randomizeM
        loop (1 :: Int)

batchSize :: Int
batchSize = 10

eta :: Double
eta = 0.0001

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
    c
    e
    (\(Attributes sl sw pl pw) -> cons sl (cons sw (cons pl (cons pw nil)))) 
    toIris

  where

    c :: Layer 4 3
    c = let l1 = tanhLayer :: Layer 4 2
            l2 = tanhLayer :: Layer 2 3
        in  l1 >>> l2 >>^ softmax

    e :: Iris -> Vector 3 Analytic -> Analytic
    e i y =
        let y' = case i of
                    Setosa     -> cons 1 (cons 0 (cons 0 nil))
                    Versicolor -> cons 0 (cons 1 (cons 0 nil))
                    Virginica  -> cons 0 (cons 0 (cons 1 nil))
            d  = (-) <$> y <*> y'
        in  d <%> d

    toIris :: Vector 3 Double -> Iris
    toIris ys = let [y0, y1, y2] = toList ys
                in if y0 >= max y1 y2
                       then Setosa
                       else if y1 >= y2 then Versicolor
                                        else Virginica
