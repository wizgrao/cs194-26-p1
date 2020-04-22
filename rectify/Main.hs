module Main where

import Lib
import P4
import P3
import System.Environment
import Graphics.Image as I
import Debug.Trace
import Data.List as L
import Text.CSV
import Data.Either
import Control.Monad

main :: IO ()
main = 
  do args <- getArgs
     img  <- readImg $ head args :: IO Im
     img2  <- readImg $ args !! 1 :: IO Im
     let rest = tail $ tail args
     let (pt1, pt2) = (toMPoints2 rest)
     let c1 = Corr img pt1
     let c2 = Corr img2 pt2
     let (Corr im3 pt3) = warp2 c1 c2
     displayImage $ drawCorrRed c1
     displayImage $ drawCorrRed c2 
     displayImageUsing defaultViewer True $ drawCorrRed $ Corr im3 pt3
     writeImage "out.jpg" im3 
     putStrLn "yuh"
