module Main where

import Lib
import P2
import System.Environment
import Graphics.Image as I

main :: IO ()
main = do args <- getArgs
          img  <- readImg $ head args
          img2 <- readImg $ args !! 1
          let img3 = (composite (read $ args !! 3) img img2)
              img4 = (lowp (read $ args !! 3) img)
              img5 = (hip (read $ args !! 3) img2)              
              fname = args !! 2
          writeImage fname  img3
          writeImage ("lowp" ++ fname) img4
          writeImage ("hip" ++ fname) img5
          writeImage ("lowpf" ++ fname) (I.normalize . fourier $ img4)
          writeImage ("hipf" ++ fname) (I.normalize . fourier $ img5)
          writeImage ("f" ++ fname) (I.normalize . fourier $ img3)

