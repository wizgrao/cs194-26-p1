module Main where

import Lib
import P2
import P3
import Quilt
import Seam
import System.Random
import System.Environment
import Graphics.Image as I

yeet :: Im -> IO ()
yeet = I.displayImageUsing defaultViewer True 

main :: IO ()
main = do args <- getArgs
          img  <- readImg $ head args
          let vert = args !! 2 == "x"
          let add = args !! 1 == "+"
          let p = read $ args !! 3 :: Int
          let yote = if add then addSeams vert p img else cutSeams vert p img
          I.writeImage (args !! 4) yote
          yeet yote
