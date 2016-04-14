module Lib.Parser where

import Lib.Prototype
import Text.Regex.PCRE.ByteString.Utils (splitCompile')
import Data.Either.Unwrap (fromRight)
import qualified Data.ByteString.Char8 as C

import Text.Regex ()
import Text.Regex.Posix.Wrap ((=~))
import Control.Arrow((&&&))
import Text.Printf(printf)

genFileName :: Int
            -> String
genFileName index = "tagged/wsj_" ++ (printf "%04d" index) ++ ".pos"

parseRawFile :: Raw
             -> Corpus
parseRawFile =
    let
    onlyValid = (=~ "^[A-Za-z0-9]+/[A-Za-z0-9]+$")
    onlyTokens = fromRight . splitCompile' (C.pack "[\n ,]")
    validSplit = fromRight . splitCompile' (C.pack "/")
    toWordTag [word_, tag_] = WordTag word_ tag_
    toWordTag _ = error "Parse Error"
    in
    map toWordTag
    . map validSplit
    . filter onlyValid
    . onlyTokens


parseFileRange :: Int
               -> Int
               -> IO Corpus
parseFileRange start end = let
                              files = map genFileName [start..end]
                           in
                           (mapM C.readFile files)
                           >>= return . concatMap parseRawFile

inputToIndividual :: Raw
                  -> Individual
inputToIndividual = map (\_word -> mkChromosome { cWord = _word })
                    . fromRight
                    . splitCompile' (C.pack "[ ]")

showIndividual :: Individual -> [(C.ByteString, C.ByteString)]
showIndividual = map (\c -> (cWord &&& maybe (C.pack "nil") fst . cTag) c) 
