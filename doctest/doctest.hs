import Test.DocTest

main :: IO ()
main = doctest [ "src/Data/Utils/Analytic.hs"
               , "src/Data/Utils/Matrix.hs"
               , "src/Data/Utils/List.hs"
               , "src/Data/Utils/Random.hs"
               , "src/Data/Utils/Statistics.hs"
               , "src/Data/Utils/Traversable.hs"
               , "src/Data/Utils/Vector.hs"
               , "src/Numeric/Neural/Normalization.hs"
               ]
