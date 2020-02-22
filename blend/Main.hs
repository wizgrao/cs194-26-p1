module Main where

import Lib
import P2
import System.Environment
import Graphics.Image as I

main :: IO ()
main = do args <- getArgs
          img  <- readImg $ head args
          img2 <- readImg $ args !! 1
          mask <- readImg $ args !! 2 
          let fname = args !! 3
          writeImage fname (multiBlend (read $ args !! 4) (read $ args !! 5) mask img img2)
          --writeImage fname (blendHalf (read $ args !! 3) (read $ args !! 4) img img2)

