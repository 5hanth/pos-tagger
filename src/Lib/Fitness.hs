module Lib.Fitness where

import Lib.Prototype
import Control.Arrow ((&&&))

reckonCFitness :: Individual -> Individual
reckonCFitness =
          let
             total :: PredictedTags
                   -> Double
             total [] = error "cTags not predicted or insufficient training data!"                      
             total xs = foldl (+) 0
                        $ map (fromInteger
                               .toInteger
                               .snd) xs

             prob :: (Maybe TagF,PredictedTags)
                  -> Double
             prob (Just (_,f), xs) = ((fromIntegral
                                         $ toInteger f) /)
                                     $ total xs
             prob _ = 0
          in map (\(c,pb)
                  -> c { cFitness = pb })
             . map (id &&& (prob
                            . (cTag &&& cTags)))

reckonIFitness :: Individual
               -> Fitness
reckonIFitness = foldl (+) 0
                 . map cFitness 
                 . reckonCFitness
                 
reckonPFitness  :: Population
                -> Population
reckonPFitness pop =
    let
    inds =  map reckonCFitness
            $ pIndividuals pop
    totFit  = foldl (+) 0
              $ map reckonIFitness inds
    in pop { pIndividuals = inds
           , pFitness     = totFit
           }
