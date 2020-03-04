module Main where

import Lib
import P3
import System.Environment
import Graphics.Image as I
import Debug.Trace
import Data.List as L

main :: IO ()
main = 
  do args <- getArgs
     img  <- readImg $ head args
     displayImage img
     displayImage $ testRaster img
     getLine >> return ()


