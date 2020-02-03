module Lib
  ( someFunc
  ) where
import Control.Applicative
import System.Environment
import System.Random
import Numeric.LinearAlgebra
import Debug.Trace
import Graphics.Image as I
import Data.Bifunctor

someFunc :: IO ()
someFunc = do args <- getArgs
              image <- readImg $ head args
              let (blueChannel, greenChannel, redChannel) = splitThirds image
                  (reconImage, gidx, ridx) = (match3Gaussian crossCorrelate redChannel greenChannel blueChannel) 
                in do
           ---       displayImage reconImage
                  displayImage . sobel $ image
                  putStrLn ("greenIdx: " ++ show gidx)
                  putStrLn ("redIdx: " ++ show ridx)
                  writeImage (args !! 1) reconImage

applyGaussian ::  Image VU RGB Double -> Image VU RGB Double
applyGaussian = I.applyFilter (I.gaussianBlur  1.0)


readImg = readImageRGB VU 

splitThirds :: Image VU RGB Double -> (Image VU RGB Double, Image VU RGB Double, Image VU RGB Double)
splitThirds image = 
  let (h, w) = dims image
      smallH = quot h 3
      newDims = (smallH, w)
      firstImage =  crop (0,0)  newDims image
      secondImage = crop (smallH, 0) newDims image
      thirdImage = crop (smallH*2, 0)  newDims image
  in (firstImage, secondImage, thirdImage)

combineRG :: Image VU RGB Double -> Image VU RGB Double -> Image VU RGB Double 
combineRG imr img = traverse2 imr img 
                        (const id)
                        (\imrAcc imgAcc  (a, b) -> 
                          let PixelRGB rVal _ _ = imrAcc (a, b)
                              PixelRGB _ gVal bVal =  imgAcc (a, b)
                          in PixelRGB rVal gVal bVal)

combineGB :: Image VU RGB Double -> Image VU RGB Double -> Image VU RGB Double 
combineGB imr img = traverse2 imr img 
                        (const id)
                        (\imrAcc imgAcc (a, b) -> 
                          let PixelRGB rVal gVal _ = imrAcc (a, b)
                              PixelRGB _ _ bVal =  imgAcc (a, b)
                          in PixelRGB rVal gVal bVal)

combineRGB imr img = combineGB (combineRG imr img) 

p :: Double -> Pixel RGB Double
p = I.PixelRGB 0.0 0.0 

sobelx :: Image VU RGB Double -> Image VU RGB Double
sobelx = I.convolve Edge  ( I.fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]])

sobely :: Image VU RGB Double -> Image VU RGB Double
sobely = I.convolve Edge (I.fromLists [[-1, -2, -1], [0, 0, 0], [1, 2, 1]])

sobel:: Image VU RGB Double -> Image VU RGB Double
sobel x = sobelx x^2 + sobely x^2

demean :: Image VU RGB Double -> Image VU RGB Double
demean meaned =  
  let (h, w) = dims meaned
      avgPix = I.sum meaned * PixelRGB (1.0/fromIntegral (h*w))  (1.0/fromIntegral (h*w)) (1.0/fromIntegral (h*w))
  in  I.map (\x -> x - avgPix) meaned

crossCorrelate :: Image VU RGB Double -> Image VU RGB Double -> Double 
crossCorrelate imLeft imRight = 
  let (h, w) = dims imLeft
      cropcrop = crop (quot h 4, quot w 4) (quot h 2, quot h 2) :: Image VU RGB Double -> Image VU RGB Double
      PixelRGB val _ _ = I.sum $ (demean.sobel.cropcrop $ imLeft)*(demean.sobel.cropcrop $ imRight)
  in -val

findTranslateMin :: (Image VU RGB Double -> Image VU RGB Double -> Double) 
                    -> Image VU RGB Double                    
                    -> (Int, Int) -> (Int, Int)
                    -> Image VU RGB Double 
                    -> ((Int, Int), Image VU RGB Double)
findTranslateMin cost base (i, j) (h, w) match = 
  let indeces = [(ii, jj) | ii <- [i..(i+h-1)], jj <- [j..(j+w-1)]]
      (idx, lost, best) = foldl (\(idx, foundMin, curBest) newIdx -> 
        let translated = I.translate Wrap newIdx match
            loss = cost base translated
        in if loss < foundMin then (newIdx, loss, translated) else (idx, foundMin, curBest))
        ((0, 0), 1e9, match)
        indeces
  in (idx, best)

match3 blGreen blRed sizeGreen sizeRed cost rChannel gChannel bChannel = 
  let findDisp = findTranslateMin cost bChannel
      (gidx, gbest) = findDisp blGreen sizeGreen  gChannel
      (ridx, rbest) = findDisp blRed sizeRed rChannel
  in (combineRGB rbest gbest bChannel, gidx, ridx)


sizeDown = I.downsample odd odd . applyGaussian 
match3Gaussian cost rChannel gChannel bChannel =
  let (h, w) = I.dims rChannel
  in if h > 64
     then let (_, gidx, ridx) = match3Gaussian cost (sizeDown rChannel) (sizeDown gChannel) (sizeDown bChannel)
          in match3 (bimap ((subtract 1).(*2)) ((subtract 1).(*2)) gidx) (bimap ((subtract 1).(*2)) ((subtract 1).(*2)) ridx) (3, 3) (3,3) cost rChannel gChannel bChannel
     else match3 (-4, -4) (-4, -4) (8, 8) (8, 8) cost rChannel gChannel bChannel 
