module P2
  ( dx
  , I.Image
  , dy
  , composite
  , fourier
  , dogx
  , dogy
  , gthendx
  , gthendy
  , toRGB
  , binarize
  , histogramGradient
  , laplacianPyramid
  , gaussianPyramid
  , findRotation
  , rotateDegrees
  , cropRotated
  , sharpen
  , lowp
  , hip
  , multiBlend
  , blendHalf
  ) where

import qualified Graphics.Image as I
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Debug.Trace
import Data.List as L
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap


toDegree :: Double -> Double -> Int
toDegree y x = round (180.0/3.14*atan2 y x)
gradientMap :: I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double ->  IntMap.IntMap Int
gradientMap im1 im2 =
  let pixels =  zip (concat . I.toLists $ im1) (concat . I.toLists $ im2)
      gradients = do (I.PixelRGB r g b, I.PixelRGB r2 g2 b2) <- pixels
                     [(toDegree r r2, 1), (toDegree g g2, 1), (toDegree b b2, 1)]
      mapList = IntMap.fromListWith (+) gradients
  in mapList

countGradients ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double ->  [(Int, Int)]
countGradients i0 i1 = IntMap.toAscList  (gradientMap i0 i1)

factor = 4 :: Int

lookupOrZero :: Int -> IntMap.IntMap Int -> Int
lookupOrZero idx imap =  fromMaybe 0 $ IntMap.lookup idx imap 

cropRotated ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
cropRotated im rotatedImage =
  let (oh, ow) = I.dims im
      (rh, rw) = I.dims rotatedImage
      cropped = I.crop (rh`div`2 - oh`div`(2*factor), rw`div`2 - ow`div`(2*factor)) (oh`div`factor, ow`div`factor) 
  in cropped

rotateDegrees :: Int -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
rotateDegrees degrees =
  let radiens = fromIntegral degrees *3.14/180.0
  in I.rotate I.Bilinear (I.Fill 0) radiens

scoreRotation :: Int -> I.Image I.VU I.RGB Double -> Int
scoreRotation degrees im =
  let rotatedImage = rotateDegrees degrees im
      cropped = cropRotated im rotatedImage
      imdx = cropped $ I.convolve I.Edge dogx rotatedImage
      imdy = cropped $ I.convolve I.Edge dogy rotatedImage
      gradMap = gradientMap imdx imdy
  in lookupOrZero 0 gradMap + lookupOrZero 90 gradMap + lookupOrZero 180 gradMap + lookupOrZero (-90) gradMap + lookupOrZero (-180) gradMap 

sig :: [(Double, Double)]
sig = [(0.0,0.0), (1.0,1.0), (2.0, 4.0)]

isBetter im curmax x = 
  let newScore = trace (show x) (scoreRotation x im, x)
  in max newScore curmax


findRotation im = foldl' (isBetter im) (0,0) [-45..45]

histogramGradient :: String -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double -> IO ()
histogramGradient fileName dxImage dyImage = toFile def fileName $ do 
  layout_title .= "Gradients"
  plot (line "Gradient (Degrees)" [countGradients dxImage dyImage])

dx ::I.Image I.VU I.X Double --- Computes the edges in the I.X direcion
dx =  I.fromLists [[1, -1]]

dy ::I.Image I.VU I.X Double --- Computes the edges in the Y direction
dy = I.fromLists [[1], [-1]]

dogx ::I.Image I.VU I.X Double 
dogx = I.convolve I.Edge dx gaussianImage
dogy ::I.Image I.VU I.X Double
dogy = I.convolve I.Edge dy gaussianImage

gthendx ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
gthendx x = I.convolve I.Edge dx (I.applyFilter (I.gaussianBlur 1.0) x)
gthendy ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
gthendy x = I.convolve I.Edge dy (I.applyFilter (I.gaussianBlur 1.0) x)


toRGBPixel :: I.Pixel I.X Double -> I.Pixel I.RGB Double
toRGBPixel (I.PixelX e) = I.PixelRGB e e e 

toRGB ::I.Image I.VU I.X Double -> I.Image I.VU I.RGB Double
toRGB = I.map toRGBPixel

maxPixels :: I.Pixel I.RGB Double -> I.Pixel I.RGB Double -> I.Pixel I.RGB Double
maxPixels (I.PixelRGB r g b) (I.PixelRGB r1 g1 b1) = 
  let maxR = max r r1
      maxG = max g g1
      maxB = max b b1
  in I.PixelRGB maxR maxG maxB 

unImpulse :: Int -> I.Image I.VU I.X Double
unImpulse semiRad= I.makeImage (2*semiRad + 1, 2*semiRad + 1) (\(i, j) -> if i == j && i == semiRad then I.PixelX 1 else I.PixelX 0)

maxImage ::I.Image I.VU I.RGB Double -> I.Pixel I.RGB Double
maxImage = I.fold maxPixels (I.PixelRGB 0 0 0)

gaussianImage ::I.Image I.VU I.X Double
gaussianImage = I.applyFilter (I.gaussianBlur 1) $ unImpulse 3

gaussianImageR :: Int -> I.Image I.VU I.X Double
gaussianImageR r = I.applyFilter (I.gaussianBlur $ fromIntegral r) $ unImpulse (3*r)

complementGaussianR :: Int -> I.Image I.VU I.X Double
complementGaussianR r = unImpulse (3*r) - gaussianImageR r 

lowp :: Int -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
lowp r = I.convolve I.Edge (gaussianImageR r) 

hip :: Int -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
hip r = I.convolve I.Edge (complementGaussianR r)

composite :: Int -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
composite r low high = I.convolve I.Edge (gaussianImageR r) low + I.convolve I.Edge (complementGaussianR r) high

binarize :: Double -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
binarize threshold = I.map (\(I.PixelRGB r _ _) -> if r < threshold then I.PixelRGB 0 0 0 else I.PixelRGB 1 1 1)

sharpen :: Double -> I.Image I.VU I.X Double
sharpen alpha = (1 + alpha) `sm` unImpulse 6 - alpha `sm` gaussianImageR 2

sm :: Double -> I.Image I.VU I.X Double -> I.Image I.VU I.X Double
sm x = I.map (\(I.PixelX y) -> I.PixelX (x*y)) 

sumStacks :: Int -> [I.Image I.VU I.RGB Double] -> [I.Image I.VU I.RGB Double] -> I.Image I.VU I.RGB Double
sumStacks n laplacianStack gaussianStack  =  L.sum (take n laplacianStack) + (gaussianStack !! n)

multiBlend s d mask im1 im2 = 
  let maskGaussian = gaussianPyramid s $ blur s mask
      invMask = gaussianPyramid s $ blur s (1 - mask)
      im1Laplacian = laplacianPyramid s im1
      im1Gaussian = gaussianPyramid s im1
      im2Laplacian = laplacianPyramid s im2
      im2Gaussian = gaussianPyramid s im2
      masked1Laplacian = L.zipWith (*) im1Laplacian maskGaussian
      masked1Gaussian = L.zipWith (*) im1Gaussian maskGaussian
      masked2Laplacian = L.zipWith (*) im2Laplacian invMask
      masked2Gaussian = L.zipWith (*) im2Gaussian invMask
      finalGaussian = L.zipWith (+) masked1Gaussian masked2Gaussian
      finalLaplacian = L.zipWith (+) masked1Laplacian masked2Laplacian
  in sumStacks d finalLaplacian finalGaussian
blendHalf s d im1 = multiBlend s d (halfFilter $ I.dims im1) im1      

power2Square ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
power2Square im = 
  let (h, w) = I.dims im
      shorter = min h w
      lg2 = floor $ logBase 2 (fromIntegral shorter)
      newDim = 2 ^ lg2
  in  I.crop (0, 0) (newDim, newDim) im 

fourier ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
fourier = fftshift. I.map ((\(I.PixelRGB x y z) -> I.PixelRGB (log x) (log y) (log z)).I.magnitude) . I.fft . toCplx . power2Square

toCplx ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB (I.Complex Double)
toCplx = I.map (I.+:0)

fftshift ::I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
fftshift img = 
  let (h, _) = I.dims img
  in I.translate I.Wrap (h `div` 2, h `div` 2) img

gaussianPyramid  :: Double -> I.Image I.VU I.RGB Double -> [I.Image I.VU I.RGB Double]
gaussianPyramid sig im = im : L.map  (blur sig) (gaussianPyramid sig im)

blur :: Double -> I.Image I.VU I.RGB Double -> I.Image I.VU I.RGB Double
blur s = I.applyFilter $ I.gaussianBlur s

laplacianPyramid  :: Double -> I.Image I.VU I.RGB Double -> [I.Image I.VU I.RGB Double]
laplacianPyramid sig im = let gp = gaussianPyramid sig im in L.zipWith (-) gp $ tail gp

halfFilter (h, w) = I.makeImageR I.VU (h, w) (\(i, j) -> if j < w `div` 2 then 1.0 else 0.0)
