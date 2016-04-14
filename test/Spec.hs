module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.ByteString.Char8 as C

import Lib.Genetic
import Lib.Utils
import Lib.Parser
import Lib.Prototype

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = hUnitTestToTests
        $ TestList [parserTests,utilsTests, tagTests]

parserTests :: Test.HUnit.Test
parserTests = TestList [ "genFileName" ~: "tagged/wsj_0042.pos" ~=? (genFileName 42)

                       , "parseRawFile" ~: do
                             let headWT = WordTag (C.pack "Pierre") (C.pack "NNP")
                             raw <- (C.readFile (genFileName 1))
                             let parsed =  parseRawFile raw
                             headWT @=? (head parsed)

                       , "inputToindividual" ~: (map C.pack ["I", "love", "you"])
                         ~=? (map cWord sampleIndividual) ]

utilsTests :: Test.HUnit.Test
utilsTests = TestList [ "freq" ~: [(1,1),(2,1),(24,1),(42,2)] ~=? (freq [42,24,2,1,42])

                      , "randElement" ~: do
                            r1 <- randElement [1..10]
                            r2 <- randElement [1..10] 
                            (r1 /= r2) @? "randElement produced same element consecutively"

                      , "genBigrams" ~: [(1,2),(2,3),(3,4)] ~=? (genBigrams [1..4])

                      , "predictWordTags" ~: do
                            let predicted = (predictWordTags parsedData sampleIndividual)
                            ((cTags $ head predicted) /= []) @? "can't predict tags for word I" ]

tagTests :: Test.HUnit.Test
tagTests = TestList (replicate 0 testTagged)
               
testTagged = "testTagged" ~:
             (any (flip elem
                   (map (mkSampleTagged ["Jack", "ate", "Apple"])
                        [ ["NNP", "VBD", "NNP"]
                        , ["NNP", "VB", "NNP"]]))
              (map showIndividual $ pIndividuals (ppi "Jack ate Apple")))
             @? "Tagging string Jack ate Apple failed"
             
sampleIndividual :: Individual
sampleIndividual = inputToIndividual $ C.pack "I love you"

mkSampleTagged ss ts =  zip (map C.pack ss) (map C.pack ts)



-- let ls = map (map reckonIFitness . pIndividuals) $ pChronicle (ppi "I love you")
-- mapM (appendFile "/tmp/f.csv" . (++ "\n"). show) ls
