import Test.DocTest

main :: IO ()
main = doctest [ "src/Neural/Normalization.hs"
               , "src/Utils/Analytic.hs"
               , "src/Utils/Matrix.hs"
               , "src/Utils/List.hs"
               , "src/Utils/Statistics.hs"
               , "src/Utils/Traversable.hs"
               , "src/Utils/Vector.hs"
               ]
