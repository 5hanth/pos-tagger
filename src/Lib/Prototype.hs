module Lib.Prototype where

import qualified Data.ByteString.Char8 as C


type WordT = C.ByteString
type TagT  = C.ByteString
type TagF  = (TagT, Int)
type PredictedTags = [TagF]             
type Raw   = C.ByteString
type Fitness = Double
               
data WordTag = WordTag
    { word :: WordT
    , tag  :: TagT
    } deriving (Show, Eq, Ord)

type Corpus = [WordTag]
              
data Chromosome = Chromosome
    { cWord         :: WordT
    , cTag          :: Maybe TagF
    , cTags         :: PredictedTags
    , cPredTags     :: PredictedTags
    , cPredWordTags :: PredictedTags
    , cSuccTags     :: PredictedTags
    , cSuccWordTags :: PredictedTags
    , cFitness      :: Fitness
    } deriving ( Eq)

type Individual = [Chromosome]

data Population = Population
    { pIndividuals :: [Individual]
    , pFitness :: Fitness
    , pProgress :: [Fitness]
    , pChronicle :: [Population]
    } --deriving (Show)

mkPopulation :: Population
mkPopulation = Population
    { pIndividuals = []
    , pFitness     = 0
    , pProgress    = []
    , pChronicle   = []
    }
    
mkChromosome :: Chromosome
mkChromosome = Chromosome
    { cWord         = C.pack ""
    , cTag          = Nothing
    , cTags         = []
    , cPredTags     = []
    , cPredWordTags = []
    , cSuccWordTags = []
    , cSuccTags     = []
    , cFitness      = 0
    }

instance Show Chromosome where
    show c = (show $ cWord c)
             ++ " "
             ++ (show $ cTag c)
