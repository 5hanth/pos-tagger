module Main where

import Lib.Genetic
import System.Environment

main :: IO ()
main =  getArgs  >>=  putStr . show . ppi . unwords 
