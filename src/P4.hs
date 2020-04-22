module P4  
  ( rectify
  , Corr (Corr)
  , warp2
  , toMPoints2
  , drawCorrRed
  ) where

import P3
import P2
import qualified Numeric.LinearAlgebra.Static as S
import GHC.TypeNats as T
import qualified Numeric.LinearAlgebra as L
import qualified Graphics.Image as I
import Debug.Trace
import Data.Maybe
import Data.Bifunctor

type MPoint = Maybe (L.Vector Double)

bw :: Im -> Im
bw = I.map ( \ (I.PixelRGB r g b) -> let lum = (0.2126*r + 0.7152*g + 0.072*b) in I.PixelRGB lum lum lum)

lumdx = I.convolve I.Edge dx . bw
lumdy = I.convolve I.Edge dy . bw

outerProductDerivatives im = I.zipWith ( \ (I.PixelRGB dx  _ _) (I.PixelRGB dy _ _) -> I.PixelRGB (dx * dx) (dy * dy) (dx * dy)) (lumdx im) (lumdy im)

harris = I.applyFilter (I.gaussianBlur 1.5) . outerProductDerivatives

cornerScore = I.map (\ (I.PixelRGB xx yy xy) -> let score = xx * yy - xy * xy in I.PixelRGB score score score) . harris



coerceMPoint :: MPoint -> L.Vector Double
coerceMPoint p = case p of
  Just a -> a
  Nothing -> trace "Yo should not be here B, your coersion failed" (L.fromList [0.0, 0.0])

data Corr = Corr Im [MPoint]

findSimilarities :: [MPoint] -> [MPoint] -> ([L.Vector Double], [L.Vector Double])
findSimilarities left right = 
  let zipped = zip left right
      filtered = filter (\(x, y) -> isJust x && isJust y) zipped
      mapped = map (bimap coerceMPoint coerceMPoint) filtered
  in  unzip mapped

translationMatrix :: Double -> Double -> L.Matrix Double
translationMatrix x y = 
  (3 L.>< 3) [ 1, 0, x
             , 0, 1, y 
             , 0, 0, 1]

toAffine :: L.Vector Double -> L.Vector Double
toAffine x = L.fromList $ L.toList x ++ [1.0]

fromAffine :: L.Vector Double -> L.Vector Double
fromAffine x = L.fromList [x `L.atIndex` 0/x `L.atIndex` 2, x `L.atIndex` 1/x `L.atIndex` 2]

stack :: [L.Vector Double] -> L.Vector Double
stack = L.fromList . concatMap L.toList

applyProjective h v = fromAffine $ h L.#> toAffine v

toHomographyRows :: L.Vector Double -> L.Vector Double -> [L.Vector Double]
toHomographyRows v v' = 
  let x = v `L.atIndex` 0
      y = v `L.atIndex` 1
      x' = v' `L.atIndex` 0
      y' = v' `L.atIndex` 1
  in [L.fromList [x, y, 1, 0, 0, 0, -x'*x, -x'*y], L.fromList [0, 0 , 0, x, y, 1, -y'*x, -y'*y]]

computeHomography :: [L.Vector Double] -> [L.Vector Double] -> L.Matrix Double 
computeHomography v v' = 
  let target = stack v' 
      mat = L.fromRows . concat $ zipWith toHomographyRows v v'
      hVector = mat L.<\> target
  in  L.reshape 3 $ toAffine hVector

fromTuple :: (Int, Int) -> L.Vector Double
fromTuple (x, y) = L.fromList [fromIntegral x, fromIntegral y]

drawPoint :: Double -> I.Pixel I.RGB Double -> L.Vector Double -> Im -> Im
drawPoint r c p = I.imap (\x p2 -> if L.norm_2 (fromTuple x - p) < r then c else p2)

drawCorr :: I.Pixel I.RGB Double -> Corr -> Im
drawCorr c (Corr im pts) = foldl (\im pt -> 
  case pt of Just x -> drawPoint 2 c x im
             _      -> im) im pts

drawCorrRed = drawCorr $ I.PixelRGB 0.7 0.2 0.2

toTuple v = (fst' v, snd' v)

fst' :: L.Vector Double -> Double
fst' v = v `L.atIndex` 0

snd' :: L.Vector Double -> Double
snd' v = v `L.atIndex` 1

toTuples :: [String] -> [(Int, Int)]
toTuples (x:y:z) = (read x, read y) : toTuples z
toTuples _ = []

toMPoints2 :: [String] -> ([MPoint], [MPoint])
toMPoints2 =
  let zipped (a:b:c:d:e) = (Just $ fromTuple (read a, read b), Just $ fromTuple (read c, read d)) : zipped e
      zipped [] = []
  in unzip . zipped    
    

rectify :: [String] -> String -> String -> Im -> Im
rectify pts h' w' =
  let tuples = toTuples pts
      h = read h'
      w = read w'
      vecs = map fromTuple tuples
      targets = map fromTuple [(0,0), (h - 1, 0), (h - 1, w - 1), (0, w -1)]
      homography = computeHomography vecs targets
  in  warp homography 

warp :: L.Matrix Double -> Im -> Im 
warp homography image =
  let (h, w) = I.dims image 
      ll     = L.fromList [0.0, 0.0] :: L.Vector Double
      lr     = L.fromList [0.0, fromIntegral w - 1.0] :: L.Vector Double
      ul     = L.fromList [fromIntegral h - 1.0, 0.0] :: L.Vector Double
      ur     = L.fromList [fromIntegral h - 1.0, fromIntegral w - 1.0] :: L.Vector Double
      ll'    = applyProjective homography ll
      lr'    = applyProjective homography lr
      ul'    = applyProjective homography ul
      ur'    = applyProjective homography ur
      minx   = minimum [fst' ll', fst' lr', fst' ul', fst' ur']
      maxx   = maximum [fst' ll', fst' lr', fst' ul', fst' ur']
      miny   = minimum [snd' ll', snd' lr', snd' ul', snd' ur']
      maxy   = maximum [snd' ll', snd' lr', snd' ul', snd' ur']
      homography' = L.pinv $ translationMatrix (-minx) (-miny) <> homography
      dims' = trace (show homography') (ceiling (maxx - minx), ceiling (maxy - miny))      
  in I.traverse image (const dims') (\f x -> I.interpolate I.Bilinear (I.Fill 0) (h, w) f (toTuple $ applyProjective homography' $ fromTuple x))

warpCorrs :: L.Matrix Double -> [MPoint] -> [MPoint]
warpCorrs hom = map . fmap  $ applyProjective hom

avgMPoint :: MPoint -> MPoint -> MPoint
avgMPoint (Just x) (Just y) = Just ((x + y) * 0.5)
avgMPoint (Just x) Nothing = Just x
avgMPoint Nothing (Just y) = Just y
avgMPoint _ _  = Nothing

warpCorrs2 :: L.Matrix Double -> L.Matrix Double -> [MPoint] -> [MPoint] -> [MPoint]
warpCorrs2 hom1 hom2 p1 p2= zipWith avgMPoint (warpCorrs hom1 p1) $ warpCorrs hom2 p2


warp2 :: Corr -> Corr -> Corr 
warp2 (Corr im1 pts1) (Corr im2 pts2) =
  let (h1, w1) = I.dims im1
      (h2, w2) = I.dims im2
      (npts1, npts2) = findSimilarities pts1 pts2
      homography = computeHomography npts2 npts1
      ll     = L.fromList [0.0, 0.0] :: L.Vector Double
      lr     = L.fromList [0.0, fromIntegral w2 - 1.0] :: L.Vector Double
      ul     = L.fromList [fromIntegral h2 - 1.0, 0.0] :: L.Vector Double
      ur     = L.fromList [fromIntegral h2 - 1.0, fromIntegral w2 - 1.0] :: L.Vector Double
      ll'    = applyProjective homography ll
      lr'    = applyProjective homography lr
      ul'    = applyProjective homography ul
      ur'    = applyProjective homography ur
      minx   = minimum [fst' ll', fst' lr', fst' ul', fst' ur', 0]
      maxx   = maximum [fst' ll', fst' lr', fst' ul', fst' ur', fromIntegral h1 ]
      miny   = minimum [snd' ll', snd' lr', snd' ul', snd' ur', 0]
      maxy   = maximum [snd' ll', snd' lr', snd' ul', snd' ur', fromIntegral w1]
      homography' = translationMatrix (-minx) (-miny) <> homography
      invHom = L.pinv homography'
      translate = translationMatrix (-minx) (-miny)
      invTranslate = translationMatrix minx miny
      dims' =  (ceiling (maxx - minx), ceiling (maxy - miny))      
      newCorrs = warpCorrs2 homography' translate pts2 pts1
      interp1 f x = I.interpolate I.Bilinear (I.Fill 0) (h1, w1) f (toTuple $ applyProjective invTranslate  $ fromTuple x)
      interp2 f x = I.interpolate I.Bilinear (I.Fill 0) (h2, w2) f (toTuple $ applyProjective invHom  $ fromTuple x)
      yuh f1 f2 x = let s1 = inScoreHom invTranslate im1 x
                        s2 = inScoreHom invHom im2 x
                        p1 = interp1 f1 x
                        p2 = interp2 f2 x
                    in  (s1 / (s1 + s2))***p1 + (s2 / (s1 + s2))***p2
      im = I.traverse2 im1 im2 (const $ const dims') yuh
  in  Corr im newCorrs 

lambda *** (I.PixelRGB r g b) = I.PixelRGB (lambda*r) (lambda*g) (lambda*b)

inScoreHom hom im x = 
  let (x', y') = toTuple $ applyProjective hom $ fromTuple x
  in inScore im x' y'

inScore :: Im -> Double -> Double -> Double
inScore im x y =
  let (h, w) = I.dims im
      hx = fromIntegral h / 2 :: Double
      hy = fromIntegral w / 2 :: Double
      xx =  abs ( x - hx) 
      yy =  abs ( y - hy) 
      xnorm = min (xx / hx) 1
      ynorm = min (yy / hy) 1
  in (1 - xnorm)*(1-ynorm) + 0.01
