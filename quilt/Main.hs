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
          let p = read $ args !! 1 :: Int
          let o = read $ args !! 2 :: Int
          let ov = read $ args !! 3 :: Int
          let eps = read $ args !! 4 :: Double
          let seam = read $ args !! 5 :: Bool
          let yote = quiltOverlap  g seam eps img (p, p) (o, o) ov
          yeet yote
          yeet $ quiltRandom g img (p,p) (o, o)
