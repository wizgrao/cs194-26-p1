module P3
  ( computeAffine 
  , testRaster
  ) where

import qualified Numeric.LinearAlgebra.Static as S
import GHC.TypeNats as T
import qualified Numeric.LinearAlgebra as L
import qualified Graphics.Image as I

computeAffine :: Triangle -> Triangle -> S.Sq 3  
computeAffine (Triangle x1 x2 x3) (Triangle y1 y2 y3) = 
  let linearPart = (S.col (y2 - y1) S.||| S.col (y3 - y1)) S.<> S.inv (S.col (x2 - x1) S.||| S.col (x3 - x1))
      translatePart = S.col $ linearPart S.#> (-x1) + y1
  in (linearPart S.||| translatePart) S.=== S.matrix [0.0, 0.0, 1.0] 

data Triangle = Triangle (S.R 2) (S.R 2) (S.R 2)

toAffine :: T.KnownNat n => S.R n -> S.R (n + 1)
toAffine x = x S.# 1.0

toLin :: S.R 3 -> S.R 2
toLin x = let (y, _) = S.split x in y

inTriangle :: Triangle -> S.R 2 -> Bool
inTriangle (Triangle x1 x2 x3) x = halfSpace x1 x2 x >= 0 && halfSpace x2 x3 x >= 0 && halfSpace x3 x1 x >=0


type Im  = I.Image I.VU I.RGB Double

halfSpace :: S.R 2 -> S.R 2 -> S.R 2 -> Double 
halfSpace p1 p2 c = 
  let x1 = L.atIndex (S.unwrap p1) 0 
      x2 = L.atIndex (S.unwrap p2) 0
      y1 = L.atIndex (S.unwrap p1) 1
      y2 = L.atIndex (S.unwrap p2) 1
      x  = L.atIndex (S.unwrap c) 0
      y  = L.atIndex (S.unwrap c) 1 
      a  = y2 - y1
      b  = x1 - x2
      const = a*x1 + b*y1
  in  a*x + b*y - const

tringle =
  Triangle 
    (S.vec2 0 0)
    (S.vec2 50 50)
    (S.vec2 98 24)

bingle = 
  Triangle
    (S.vec2 0 99)
    (S.vec2 99 0)
    (S.vec2 0 0) 

toVector :: (Int, Int)  -> S.R 2
toVector (a, b) = S.vector [fromIntegral a, fromIntegral b]

toTuple :: S.R 2 -> (Double, Double)
toTuple x = 
  let unwrapped =  S.unwrap x
  in (L.atIndex unwrapped 0, L.atIndex unwrapped 1)

rasterImage :: Im 
rasterImage = I.makeImage (100,100) (\x -> if inTriangle tringle $ toVector x then I.PixelRGB 1.0 1.0 1.0 else I.PixelRGB 0.0 0.0 0.0)

black = I.PixelRGB 0.0 0.0 0.0

rasterTriangle :: Triangle -> Triangle ->  Im -> Im
rasterTriangle so dest im = 
  let transform = computeAffine dest so
  in I.traverse im id  (\f x -> if inTriangle dest $ toVector x then I.interpolate I.Bilinear  I.Edge (I.dims im) f $ toTuple $ toLin (transform S.#> toAffine (toVector x)) else black) 

testRaster = rasterTriangle tringle bingle 
