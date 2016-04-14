module Lib.Utils where

import Lib.Prototype

import Data.MultiSet (toOccurList,fromList)
import Data.List(nub)
import Control.Monad (ap)
import Data.Random(runRVar)
import Data.Random.Source.DevRandom
import Data.Random.Extras(choice)


freq :: Ord a => [a] -> [(a,Int)]
freq = toOccurList . fromList

randElement :: [a]
            -> IO a
randElement xs = runRVar (choice xs) DevURandom
                 
genBigrams :: [a] -> [(a,a)]
genBigrams = ap zip tail

assignRandTag :: Chromosome
              -> IO Chromosome
assignRandTag chromo = do
    case cTags chromo of
        [] -> return chromo
        xs -> do
            tag_ <- randElement xs
            gonnaRand <- randElement (True:(replicate (length xs) False))
            orRand <- randElement [True, False]
            return $ case cTag chromo of
                         Nothing -> chromo { cTag = Just tag_ }
                         Just (_,f) -> if and [gonnaRand, or [orRand, ((f >=).snd) tag_]]
                                       then chromo { cTag = Just tag_ }
                                       else chromo

predictWordTags :: Corpus
                -> Individual
                -> Individual
predictWordTags corpus individual = 
    let
    bigrams = genBigrams corpus

    cWordTags :: Chromosome
              -> PredictedTags
    cWordTags chromo = freq
                       . map tag
                       $ filter ((== (cWord chromo))
                                    . word) corpus 
    succWordTags :: Chromosome
                 -> PredictedTags
    succWordTags chromo =  freq
                           . map (tag . snd)
                           $ filter ((== (cWord chromo))
                                     . word
                                     . fst) bigrams
                           
    predWordTags :: Chromosome
                 -> PredictedTags
    predWordTags chromo =  freq
                           . map (tag . fst)
                           $ filter ((== (cWord chromo))
                                     . word
                                     . snd) bigrams

    in map (\chromo -> chromo { cTags         = cWordTags chromo
                              , cPredWordTags = predWordTags chromo
                              , cSuccWordTags = succWordTags chromo })
           individual                             
                  
predictTags :: Corpus
            -> Individual
            -> Individual
predictTags corpus individual =
    let
    bigrams = genBigrams corpus

    succTags :: Chromosome
             -> PredictedTags
    succTags chromo = case (cTag chromo) of
        Just (tagT,_) -> freq
                         . map (tag . snd)
                         $ filter ((==tagT)
                                   . tag
                                   . fst) bigrams
        Nothing       -> []

    predTags :: Chromosome
             -> PredictedTags
    predTags chromo = case (cTag chromo) of
        Just (tagT,_) -> freq
                         . map (tag . fst)
                         $ filter ((==tagT)
                                   . tag
                                   . snd) bigrams
        Nothing       -> []

    in map (\chromo -> chromo { cPredTags     = predTags chromo
                              , cSuccTags     = succTags chromo })
           individual
    
assignCTags :: Individual
            -> Individual
assignCTags individual = let
                 bigrams ind = zip ind (tail ind)
                 assignFCTags :: (Chromosome,Chromosome)
                              -> Chromosome
                 assignFCTags (c,n) = n { cTags = nub $ concat
                                                  [ cTags n
                                                  , cSuccTags c] }
                 assignBCTags :: (Chromosome,Chromosome)
                              -> Chromosome
                 assignBCTags (c,n) = c { cTags = nub $ concat
                                                  [ cTags c
                                                  , cPredTags n] }

                 fcTagged = (head individual)
                              :(map assignFCTags
                                $ bigrams individual)
                 lst = last fcTagged
                 in (++ [lst])
                    . map assignBCTags
                    . bigrams
                    $ fcTagged


addChronicle :: Population
             -> Population
addChronicle pop = pop { pChronicle = pop:(pChronicle pop) }
                   

