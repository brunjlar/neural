import Test.DocTest

main :: IO ()
main = doctest [ "src/Utils/Matrix.hs"
               , "src/Utils/Vector.hs"
               ]
