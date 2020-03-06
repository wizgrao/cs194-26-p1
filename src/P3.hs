module P3
  ( computeAffine 
  , csvToTriangles
  , interpImages 
  , interpTriangles
  , cheeseError
  , Im
  , warpTriangles
  , averageTriangles
  , avgImages
  ) where


import qualified Numeric.LinearAlgebra.Static as S
import GHC.TypeNats as T
import qualified Numeric.LinearAlgebra as L
import qualified Graphics.Image as I
import Data.Either
import Text.CSV
import Text.Parsec.Error

cheeseError :: Either ParseError CSV -> CSV
cheeseError (Left _) = []
cheeseError (Right csv) = csv

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
inTriangle (Triangle x1 x2 x3) x = (halfSpace x1 x2 x >= 0 && halfSpace x2 x3 x >= 0 && halfSpace x3 x1 x >=0) || (halfSpace x1 x2 x <= 0 && halfSpace x2 x3 x <= 0 && halfSpace x3 x1 x <=0)

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
    (S.vec2 200 200)
    (S.vec2 300 100)

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

black = I.PixelRGB 0.0 0.0 0.0

rasterTriangle :: Im -> Triangle -> Triangle  -> Im
rasterTriangle im so dest = 
  let transform = computeAffine dest so
  in I.traverse im id  (\f x -> if inTriangle dest $ toVector x then I.interpolate I.Bilinear  I.Edge (I.dims im) f $ toTuple $ toLin (transform S.#> toAffine (toVector x)) else black) 

warpTriangles :: Im -> [Triangle] -> [Triangle] -> Im 
warpTriangles im so dest = sum $ zipWith (rasterTriangle im) so dest 

triangleFromString :: [String] -> Triangle
triangleFromString (fst:snd:thrd:frth:ffth:sxth:_) = Triangle (vfs snd fst) (vfs frth thrd) (vfs sxth ffth)

csvToTriangles :: [[String]] -> [Triangle]
csvToTriangles x = map triangleFromString (take (length x - 1) x)

vfs :: String -> String -> S.R 2
vfs a b = S.vector [read a, read b]

interp :: Double -> Triangle -> Triangle -> Triangle
interp x (Triangle a1 b1 c1) (Triangle a2 b2 c2) = Triangle (scale x a2 + scale (1-x) a1) (scale x b2 + scale (1-x) b1) (scale x c2 + scale (1-x) c1)

interpTriangles :: Double -> [Triangle] -> [Triangle] -> [Triangle]
interpTriangles x = zipWith $ interp x

scale ::Double -> S.R 2 -> S.R 2
scale a = (S.#>) $ S.matrix [a, 0, 0, a]

x |*> Triangle a2 b2 c2 = Triangle (x*a2) (x*b2) (x*c2)


(+>) :: Triangle -> Triangle -> Triangle
Triangle a1 b1 c1 +> Triangle a2 b2 c2 = Triangle (a1 + a2) (b1 + b2) (c1 + c2)

zero = Triangle 0 0 0

sumTriangles :: [[Triangle]] -> [Triangle]
sumTriangles = foldl1 $ zipWith (+>) 

averageTriangles :: [[Triangle]] -> [Triangle]
averageTriangles x = map ((1.0 / fromIntegral (length x))|*>) $ sumTriangles x 

interpImages :: Double -> Im -> [Triangle] -> Im -> [Triangle] -> Im
interpImages a im1 t1 im2 t2 = 
  let triangleSet = interpTriangles a t1 t2
      firstWarped = warpTriangles im1 t1 triangleSet 
      secondWarped = warpTriangles im2 t2 triangleSet
  in iscale (1-a) firstWarped  + iscale a secondWarped

iscale :: Double -> Im -> Im
iscale x = I.map  (\(I.PixelRGB r g b) -> I.PixelRGB (r*x) (g*x) (b*x))

avgImages :: [Im] -> Im
avgImages imgs = iscale (1.0 / (fromIntegral $ length imgs)) . sum $ imgs 
