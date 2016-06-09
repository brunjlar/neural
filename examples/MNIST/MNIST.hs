module Main where

import           Codec.Picture
import qualified Data.Array     as A
import           Data.MyPrelude
import           Data.Utils
import           Numeric.Neural
import           Pipes.GZip     (decompress)
import qualified Pipes.Prelude  as P

main :: IO ()
main = runSafeT (runEffect $ trainSamples >-> P.take 50 >-> consumeSamples "test")

type Img = Image Pixel8

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

type Sample = (Img, Digit)

trainImagesFile, trainLabelsFile, testImagesFile, testLabelsFile :: FilePath
trainImagesFile = "examples" </> "MNIST" </> "train-images-idx3-ubyte" <.> "gz"
trainLabelsFile = "examples" </> "MNIST" </> "train-labels-idx1-ubyte" <.> "gz"
testImagesFile  = "examples" </> "MNIST" </> "t10k-images-idx3-ubyte"  <.> "gz"
testLabelsFile  = "examples" </> "MNIST" </> "t10k-labels-idx1-ubyte"  <.> "gz"

bytes :: (MonadSafe m, MonadIO m) => FilePath -> Producer Word8 m ()
bytes f = decompress (fromFile f) >-> toWord8

labels :: (MonadSafe m, MonadIO m) => FilePath -> Producer Digit m ()
labels f = bytes f >-> P.drop 8 >-> P.map (toEnum . fromIntegral)

images :: (MonadSafe m, MonadIO m) => FilePath -> Producer Img m ()
images f = bytes f >-> P.drop 16 >-> chunks (28 * 28) >-> P.map g

  where

    g xs = let a = A.listArray ((0, 0), (27, 27)) xs
           in  generateImage (\x y -> 255 - a A.! (y, x)) 28 28

trainSamples, testSamples :: (MonadSafe m, MonadIO m) => Producer Sample m ()
trainSamples = P.zip (images trainImagesFile) (labels trainLabelsFile)
testSamples  = P.zip (images testImagesFile)  (labels testLabelsFile)

writeImg :: MonadIO m => FilePath -> Img -> m ()
writeImg f i = liftIO $ saveTiffImage (f <.> "tiff") (ImageY8 i)

consumeSamples :: MonadIO m => String -> Consumer Sample m ()
consumeSamples f = g (1 :: Int) where

    g i = do
        (img, l) <- await
        writeImg (printf "%s_%05d_%d" f i (fromEnum l)) img
        g (succ i)
