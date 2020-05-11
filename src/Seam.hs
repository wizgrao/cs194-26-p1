module Seam  
  ( minCutSeam,
    addSeams,
    cutSeams
  ) where

import P3
import P2
import P4
import Quilt
import qualified Numeric.LinearAlgebra.Static as S
import qualified Numeric.LinearAlgebra as L
import Data.List
import qualified Graphics.Image as I
import Debug.Trace
import Data.Maybe
import Data.Bifunctor
import Data.Array
import System.Random
import qualified Graphics.Image.Interface as II


minCutSeam :: Bool -> Im -> ([Int], Im)
minCutSeam vert im = 
  let (h, w) = I.dims im
      pow = I.sobelOperator im
      seam = calcSeam vert (if vert then w else h) pow
      newDims = if vert then (h, w-1) else (h-1, w)
      before (i, j) = if vert then j < (seam !! i) else i < (seam !! j)
  in (seam, I.traverse im (const newDims) (\getter (i, j) -> 
                            if before (i, j) 
                            then getter (i, j) 
                            else (if vert then getter (i, j+1) else getter (i+1, j))))

cutSeams vert n = (!!n) . iterate (snd . minCutSeam vert)
getSeams vert n im =  correctSeams .
                      map fst . 
                      scanr(\x y -> x $ snd y) ([],im) .
                      replicate n $ 
                      minCutSeam vert 

addSeams vert n im = foldr (addSeam vert) im (getSeams vert n im)

avg (I.PixelRGB r g b) (I.PixelRGB r1 g1 b1) = I.PixelRGB (0.5*r + 0.5*r1) (0.5*g + 0.5*g1) (0.5*b + 0.5*b1)

combSeam :: [Int] -> [Int] -> [Int]
combSeam = zipWith (\x y -> if y > x then y+2 else y)

addSeam :: Bool  -> [Int] -> Im-> Im
addSeam vert seam im = 
  let (h, w) = I.dims im
      newDims = if vert then (h, w+1) else (h+1, w)
      before (i, j) = if vert then j < (seam !! i) else i < (seam !! j)
      is (i, j) = if vert then j == (seam !! i) else i == (seam !! j)
      avgAdj getter (i, j) = if vert then avg (getter (i, j))  (getter (i, j+1))  else avg (getter (i, j))  (getter (i+1, j))
  in I.traverse im (const newDims) (\getter (i, j) -> case () of 
                _ | before (i, j) -> getter (i, j)
                  | is (i, j) -> avgAdj getter (i, j)
                  | otherwise -> if vert then getter (i, j-1) else getter (i-1, j)) 

correctSeams :: [[Int]] -> [[Int]]
correctSeams [x,[]] = [x] 
correctSeams (x:y) = let y' = correctSeams y in foldr combSeam x y' : y'
