module Main where

import Lib
import P2
import P3
import Quilt
import System.Random
import System.Environment
import Graphics.Image as I

yeet :: Im -> IO ()
yeet = I.displayImageUsing defaultViewer True 

main :: IO ()
main = do args <- getArgs
          g <- getStdGen
          img  <- readImg $ head args
          let o = args !! 1 
          transfer <- readImg o
          let p = read $ args !! 2 :: Int
          let ov = read $ args !! 3 :: Int
          let eps = read $ args !! 4 :: Double
          let alph = read $ args !! 5 :: Double
          let yote = quiltOverlapIm g eps alph img transfer (p, p) ov 
          I.writeImage (args !! 6) yote
          yeet yote
