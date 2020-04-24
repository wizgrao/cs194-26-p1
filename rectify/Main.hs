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
import Prelude as P
import System.Random

main :: IO ()
main = 
  do args <- getArgs
     im1  <- readImg $ head args
     im2  <- readImg $ args !! 1
     let (c1, c2) = findCorr2 0.2 im1 im2
     g <- getStdGen 
     let (cc1, cc2) = ransac g 2 1000 c1 c2
     let warped = warp2 cc1 cc2
     let warped2 = warp2 cc2 cc1
     let Corr pan1 _ = warped
     let Corr pan2 _ = warped2
     displayImageUsing defaultViewer True $ drawCorrRed cc1
     displayImageUsing defaultViewer True $ drawCorrRed cc2
     displayImageUsing defaultViewer True $ drawCorrRed warped
     displayImageUsing defaultViewer True $ drawCorrRed warped2
     I.writeImage (args!!2) pan1
     I.writeImage (args!!3) pan2
