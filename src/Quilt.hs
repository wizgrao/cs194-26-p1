module Quilt  
  ( quiltRandom,
    quiltOverlap,
    quiltOverlapIm,
    calcSeam
  ) where

import P3
import P2
import P4
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

pI :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
pI (patchH, patchW) (outH, outW) ol = [(i, j) | i <- [0, patchH-ol..(outH-patchH)],
                                                       j <- [0, patchW-ol..(outW-patchW)]]
quiltRandom :: StdGen -> Im -> (Int, Int) -> (Int, Int) -> Im
quiltRandom gen texture (patchH, patchW) (outH, outW) =
  let (inH, inW) = I.dims texture 
      outIndexes = pI (patchH, patchW) (outH, outW) 0
      patchIndexes = modList2 (inH - patchH) (inW - patchW) $ randoms2 gen
      patches = map (\pos -> I.crop pos (patchH, patchW) texture) patchIndexes
      indxPatches = zip outIndexes patches 
      quilts = map (\(loc, patch) -> trans loc (outH, outW) patch) indxPatches
  in  sum quilts

quiltOverlap :: StdGen -> Bool -> Double -> Im -> (Int, Int) -> (Int, Int) -> Int -> Im
quiltOverlap gen seam eps texture (patchH, patchW) (outH, outW) overlap = 
  let (inH, inW) = I.dims texture
      features = [I.crop (i, j) (patchH, patchW) texture | i <- [0..(inH - patchH)],
                                                           j <- [0..(inW - patchW)]]
      featArray = listArray (0, length features - 1) features
      topMap = map (I.imap (\(i, j) c -> if i < overlap then c else 0)) features
      leftMap = map (I.imap (\(i, j) c -> if j < overlap then c else 0)) features
      topLeftMap = map (I.imap (\(i, j) c -> if j < overlap || i < overlap then c else 0)) features
      tf = map toVector topMap
      lf = map toVector leftMap
      tlf = map toVector topLeftMap
      outIndexes = pI (patchH, patchW) (outH, outW) overlap
      (ii, nextGen) = next gen
      start = trans (0, 0) (outH, outW) $ featArray ! (ii `mod` length features)
      match = if seam then findMatchSeam overlap else findMatch
      (im, _) = foldl' (match eps tf lf tlf featArray (patchH, patchW)) (start, nextGen) outIndexes
      in im 

group2 :: [b] -> [(b,b)]
group2 (a : b : y) = (a, b) : group2 y
group2 _ = []

findMatch ::  Double -> [L.Vector Double] ->[L.Vector Double] ->[L.Vector Double] -> Array Int Im -> (Int, Int) ->  (Im, StdGen) -> (Int, Int) -> (Im, StdGen)
findMatch eps tf lf tlf images (ph, pw)  (curImage, gen) (i, j) =
  let ptch =  I.crop (i, j) (ph, pw) curImage
      ptchVec = [toVector ptch]
      distances = concat . L.toLists $ vectorDistances ptchVec (case  (i, j) of (_, 0) -> tf
                                                                                (0, _) -> lf
                                                                                _ -> tlf)
      min = minimum distances
      indices = map fst $ filter (\(_, s) -> s <= (min + 0.01) * (1 + eps)) $ zip [0..] distances
      (idx, nextGen) = next gen 
      index = indices !! (idx `mod` length indices)
      
      finalImage = images ! index
      retImage = I.traverse2 finalImage curImage (const id) 
          (\getter1 getter2 (ii, jj) -> if picIn finalImage (ii - i, jj - j) then getter1 (ii - i, jj - j) else getter2(ii, jj))
  in (retImage, nextGen)


findMatchSeam :: Int -> Double -> [L.Vector Double] ->[L.Vector Double] ->[L.Vector Double] -> Array Int Im -> (Int, Int) ->  (Im, StdGen) -> (Int, Int) -> (Im, StdGen)
findMatchSeam ol eps tf lf tlf images (ph, pw)  (curImage, gen) (i, j) =
  let ptch =  I.crop (i, j) (ph, pw) curImage
      ptchVec = [toVector ptch]
      distances = concat . L.toLists $ vectorDistances ptchVec (case  (i, j) of (_, 0) -> tf
                                                                                (0, _) -> lf
                                                                                _ -> tlf)
      min = minimum distances
      indices = map fst $ filter (\(_, s) -> s <= (min + 0.01) * (1 + eps)) $ zip [0..] distances
      (idx, nextGen) = next gen 
      index = indices !! (idx `mod` length indices)
      finalImage = images ! index
      retImage = embedWithSeam ptch (i, j) ol curImage finalImage 
  in (retImage, nextGen)

quiltOverlapIm :: StdGen -> Double -> Double -> Im -> Im -> (Int, Int) -> Int -> Im
quiltOverlapIm gen eps alpha texture base (patchH, patchW) overlap = 
  let (inH, inW) = I.dims texture
      (outH, outW) = I.dims base
      features = [I.crop (i, j) (patchH, patchW) texture | i <- [0..(inH - patchH)],
                                                           j <- [0..(inW - patchW)]]
      featArray = listArray (0, length features - 1) features
      topMap = map (I.imap (\(i, j) c -> if i < overlap then c else 0)) features
      leftMap = map (I.imap (\(i, j) c -> if j < overlap then c else 0)) features
      topLeftMap = map (I.imap (\(i, j) c -> if j < overlap || i < overlap then c else 0)) features
      tf = map toVector topMap
      lf = map toVector leftMap
      tlf = map toVector topLeftMap
      f = map toVector features
      outIndexes = pI (patchH, patchW) (outH, outW) overlap
      (ii, nextGen) = next gen
      start = trans (0, 0) (outH, outW) $ featArray ! (ii `mod` length features)
      (im, _) = foldl' (findMatchSeamImage overlap base alpha eps tf lf tlf f featArray (patchH, patchW)) (start, nextGen) outIndexes
      in im 

findMatchSeamImage :: Int -> Im -> Double -> Double -> [L.Vector Double] ->[L.Vector Double] ->[L.Vector Double] -> [L.Vector Double] -> Array Int Im -> (Int, Int) ->  (Im, StdGen) -> (Int, Int) -> (Im, StdGen)
findMatchSeamImage ol base alpha eps tf lf tlf f images (ph, pw)  (curImage, gen) (i, j) =
  let ptch =  I.crop (i, j) (ph, pw) curImage
      ref = I.crop (i, j) (ph, pw) base
      ptchVec = [toVector ptch]
      feature = case (i, j) of (_, 0) -> tf
                               (0, _) -> lf
                               _      -> tlf
      distances = concat . L.toLists $ L.scale alpha (vectorDistances [toVector ref] f) + L.scale (1 - alpha) (vectorDistances ptchVec feature)
      min = minimum distances
      indices = map fst $ filter (\(_, s) -> s <= (min + 0.01) * (1 + eps)) $ zip [0..] distances
      (idx, nextGen) = next gen 
      index = indices !! (idx `mod` length indices)
      finalImage = images ! index
      retImage = embedWithSeam ptch (i, j) ol curImage finalImage 
  in (retImage, nextGen)

embedWithSeam ptch (i, j) ol curImage finalImage=
  case  (i, j) of 
    (0, 0) -> I.traverse2 finalImage curImage (const id) 
                (\getter1 getter2 (ii, jj) -> if picIn finalImage (ii - i, jj - j) then getter1 (ii - i, jj - j) else getter2(ii, jj))
    (_, 0) -> let topSeam = seam False ol finalImage ptch
               in I.traverse2 finalImage curImage (const id) 
                    (\getter1 getter2 (ii, jj) -> if picIn finalImage (ii - i, jj - j) && 
                                                     ii - i > topSeam !! (jj - j)
                                                  then getter1 (ii - i, jj - j)
                                                  else getter2(ii, jj))
    (0, _) ->  let leftSeam = seam True ol finalImage ptch
               in I.traverse2 finalImage curImage (const id) 
                    (\getter1 getter2 (ii, jj) -> if picIn finalImage (ii - i, jj - j) && 
                                                     jj - j > leftSeam !! (ii - i)
                                                  then getter1 (ii - i, jj - j)
                                                  else getter2(ii, jj))
    _      -> let leftSeam = seam True ol finalImage ptch
                  topSeam = seam False ol finalImage ptch
               in I.traverse2 finalImage curImage (const id) 
                    (\getter1 getter2 (ii, jj) -> if picIn finalImage (ii - i, jj - j) && 
                                                     jj - j > leftSeam !! (ii - i) &&
                                                     ii - i > topSeam !! (jj - j)
                                                  then getter1 (ii - i, jj - j)
                                                  else getter2(ii, jj))

randoms2 ::(RandomGen g, Random r) => g -> [(r, r)]
randoms2 = group2 . randoms

modList2 a b = map (bimap ( `mod` a) ( `mod` b))

trans :: (Int, Int) -> (Int, Int) -> Im -> Im
trans (i, j) dims im =
  I.traverse
    im
    (const dims)  
    (\getter (i', j') -> inOrBlack im getter (i' - i, j' - j))

inOrBlack im getter loc = if picIn im loc then getter loc else 0

picIn im (i, j) =
  let (h, w) = I.dims im
  in i >= 0 && i < h && j >= 0 && j < w 

pixPower :: Im -> (Int, Int) -> Double
pixPower im lmao = 
  let (I.PixelRGB r g b) = I.defaultIndex 0 im lmao 
  in r + g + b

calcSeam :: Bool -> Int -> Im -> [Int]
calcSeam vert ov yeet = 
  let (h, w) = I.dims yeet
      init = take ov [0..]
      seamStep = if vert then vertSeamStep else horizSeamStep
      dep = if vert then h else w
      firstStep = vertSeamStep ov yeet (dep - 1) init
      energies = scanr (vertSeamStep ov yeet) firstStep [0..dep-2]  
      fstIndex = argMin $ head energies      
      indices = scanl' seamBackStep fstIndex $ tail energies
  in indices

seam :: Bool -> Int -> Im -> Im -> [Int]
seam vert ov im1 im2 = 
  let yeet = (im1 - im2) * (im1 - im2)
  in calcSeam vert ov yeet

vertSeamStep :: Int -> Im -> Int -> [Double] -> [Double]
vertSeamStep ov im i prev =  
  map (\j -> minimum 
    [(prev !!! (j-1)) + pixPower im (i, j),
     (prev !!! j) + pixPower im (i, j),
     (prev !!! (j+1)) + pixPower im (i, j)])
    [0..ov-1]

horizSeamStep :: Int -> Im -> Int -> [Double] -> [Double]
horizSeamStep ov im j prev =  
  map (\i -> minimum 
    [(prev !!! (j-1)) + pixPower im (i, j),
     (prev !!! j) + pixPower im (i, j),
     (prev !!! (j+1)) + pixPower im (i, j)])
    [0..ov-1]

seamBackStep prev lst = -1 + prev + argMin [lst !!! (prev - 1), lst !!! prev, lst !!! (prev + 1)]

argMin :: [Double] -> Int
argMin = fst . minimumBy (\(_, i) (_, j) -> compare i j) . zip [0..]

(!!!) :: [Double] -> Int -> Double
(!!!) l i = if i < 0 || i >= length l then 10000000 else l !! i
