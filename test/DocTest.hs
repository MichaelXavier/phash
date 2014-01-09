import Test.DocTest

main = doctest [ "-isrc"
               , "-lpHash"
               , "src/Data/PHash.hs" ]
