import Test.DocTest

main :: IO ()
main = doctest [ "src/Neural/Normalization.hs"
               , "src/Utils/Matrix.hs"
               , "src/Utils/Traversable.hs"
               , "src/Utils/Vector.hs"
               ]
