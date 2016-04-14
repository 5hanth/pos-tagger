module Lib.Genetic where

import Lib.Utils
import Lib.Prototype
import Lib.Fitness
import Lib.Parser

import qualified Data.ByteString.Char8 as C
import Control.Monad (liftM)
import Data.List(intercalate,sortOn)
import Control.Arrow ((&&&))
import System.IO.Unsafe(unsafePerformIO)

reckonProgress :: Population
               -> Population
reckonProgress pop =
    let pFit = reckonPFitness pop
    in pFit { pProgress = (pFitness pFit):(pProgress pFit) }

isProgressive :: Population
              -> Bool
isProgressive pop =
    case pProgress pop of
        []  -> True
        [_] -> True
        ps  -> not
               . any (>= (length $ pIndividuals pop))
               $ map (\fit -> pred . length
                              $ filter (<= fit) ps) ps
initPopulation :: Raw
               -> Int
               -> Corpus
               -> IO Population
initPopulation raw size corpus =
    liftM reckonPFitness $
    (\inds -> mkPopulation
        { pIndividuals = inds
        , pFitness = 0
        }) <$> (sequence
                . take size
                . repeat
                . (>>= mapM assignRandTag)
                . liftM assignCTags
                . liftM (predictTags corpus)
                . mapM assignRandTag
                . predictWordTags corpus
                $ inputToIndividual raw)

crossOver :: (Individual, Individual)
          -> (Individual, Individual)
crossOver (xx,xy) =
    let
    mate :: (Chromosome,Chromosome)
         -> (Chromosome,Chromosome)
    mate (c1,c2)
        | (cFitness c1) > (cFitness c2) = (c1,c2)
        | otherwise = (c2,c1)
    in unzip
       . map mate
       $ zip xx xy

mutate :: Population
       -> IO Population
mutate pop =
    let
    mutateI :: Individual
            -> IO Individual
    mutateI ind = 
        do
            gonnaMutate <- randElement [True,False]
            case gonnaMutate of
                True  -> mapM assignRandTag ind
                False -> return ind
    in (\inds -> pop { pIndividuals = inds })
        <$> mapM mutateI (pIndividuals pop)


initGenetic :: Population
            -> IO Population
initGenetic initPop = 
    let doGenetic :: Population
                  -> IO Population
        doGenetic pop =
            case isProgressive pop of
                True -> (>>= doGenetic)
                        . mutate
                        . evolve
                        . reckonProgress 
                        $ addChronicle pop
                False -> return pop
    in doGenetic initPop

evolve :: Population
       -> Population
evolve pop =  case sortOn reckonIFitness $ pIndividuals pop of
    (i1:i2:iS) -> (\(nI1,nI2) ->
        pop { pIndividuals = nI1:nI2:iS })
        $ crossOver (i1,i2)
    _ -> pop



instance Show Population where
    show p = (intercalate "\n"
              . map show
              $ map (id &&& reckonIFitness)
                    (pIndividuals p))
             ++ "\n" ++ (show $ pFitness p)
             ++ "\n" ++ (show $ pProgress p)
--             ++ "\n" ++ (show $ pChronicle p)



-- Test

parsedData :: Corpus
parsedData = unsafePerformIO (parseFileRange 1 199)

s :: Raw
s = C.pack "I love you"


populationSize :: Int
populationSize = 20

-- i :: IO Individual
-- i = parsedData >>= \c ->
--     mapM assignRandTag
--     . predictTags c
--     . predictWordTags c
--     $ inputToIndividual s

p :: Population
p = unsafePerformIO $ initPopulation s populationSize parsedData 

pp :: String -> Population
pp ss = unsafePerformIO $ initPopulation (C.pack ss) populationSize parsedData 

ppi :: String -> Population
ppi ss = unsafePerformIO $ initGenetic (pp ss)
