module Main where

import Lib
import P2
import System.Environment
import Graphics.Image as I
import Debug.Trace
import Data.List as L

main :: IO ()
main = do args <- getArgs
          img  <- readImg $ head args
          let imagedx = (I.convolve Edge dx img)
              imagedy = (I.convolve Edge dy img)
              gtdx = trace "gtdx" $ gthendx img
              gtdy = gthendy img
              imagedogx = cropRotated img img $ I.convolve Edge dogx img
              imagedogy = cropRotated img img $ I.convolve Edge dogy img
              mag= imagedx^2 + imagedy^2
              gtmag = gtdx^2 + gtdy^2
              dogmag = imagedogx^2 + imagedogy^2
              (hist, degree) = findRotation img
              rotImage = rotateDegrees degree img
              rotDogx = cropRotated img rotImage $ I.convolve Edge dogx rotImage
              rotDogy = cropRotated img rotImage $ I.convolve Edge dogy rotImage
              sharpenedImage = I.convolve Edge (sharpen $ read (args !! 3)) img
          --histogramGradient ((args !! 1) ++ "hist.png") imagedogx imagedogy 
          --writeImage ((args !! 1) ++ "dx.png") (I.normalize imagedx)
          --writeImage ((args !! 1) ++ "dy.png") (I.normalize imagedy)
          --writeImage ((args !! 1) ++ "mag.png") (I.normalize mag)
          --writeImage ((args !! 1) ++ "magbin.png") (binarize (read (args !! 2)) mag)
          --writeImage ((args !! 1) ++ "gtmagbin.png") (binarize (read (args !! 2)) gtmag)
          --writeImage ((args !! 1) ++ "gtdx.png") (I.normalize gtdx)
          --writeImage ((args !! 1) ++ "gtdy.png") (I.normalize gtdy)
          --writeImage ((args !! 1) ++ "dogdx.png") (I.normalize imagedogx)
          --writeImage ((args !! 1) ++ "dogdy.png") (I.normalize imagedogy)
          --writeImage ((args !! 1) ++ "gtmag.png") (I.normalize gtmag)
          --writeImage ((args !! 1) ++ "dogmag.png") (I.normalize dogmag)
          --writeImage ((args !! 1) ++ "dogx.png") (I.pixelGrid 10  (I.normalize (toRGB dogx)))
          --writeImage ((args !! 1) ++ "fourier.png") ( I.normalize . fourier $ img)
          --writeImage ((args !! 1) ++ "straight.png") rotImage
          --writeImage ((args !! 1) ++ "sharp.png") sharpenedImage
          --histogramGradient ((args !! 1) ++ "histstraight.png") rotDogx rotDogy 
          sequence (L.map (\(i,l) -> writeImage ((args !! 1) ++ show i ++ "gauss.png") l) $ zip [0..15] $ take 16 $ gaussianPyramid 5 img) 
          sequence (L.map (\(i,l) -> writeImage ((args !! 1) ++ show i ++ "lapl.png") $ normalize l) $ zip [0..15] $ take 16 $ laplacianPyramid 5 img) 
          return ()

