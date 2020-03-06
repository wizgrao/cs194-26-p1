module Main where

import Lib
import P3
import System.Environment
import Graphics.Image as I
import Debug.Trace
import Data.List as L
import Text.CSV
import Data.Either
import Control.Monad

main :: IO ()
main = 
  do args <- getArgs
     eitherCSV1 <- parseCSVFromFile $ args !! 1
     img1 <- readImg $ head args 
     --print eitherCSV1
     let csv1 = cheeseError eitherCSV1
     eitherCSV2 <- mapM (\x -> parseCSVFromFile $ "frontalImgs/" ++ show x ++ "a.csv") [1 .. 100]
     imgs <- mapM (\x -> readImg $ "frontalImgs/" ++ show x ++ "a.jpg") [1 .. 100]
     let csv2 = L.map cheeseError eitherCSV2
     ---if null csv1 || null csv2 then putStrLn "Error Loading CSV" else putStrLn "csvs loaded!"
     let tringle1 = csvToTriangles csv1
     let tringle2 = L.map csvToTriangles csv2
     let avgTriangles = averageTriangles tringle2
     let warped = L.zipWith (\im triangles -> warpTriangles im triangles avgTriangles) imgs tringle2 
     let avgFace = avgImages warped
     writeImage "toAverage.jpg"  $ warpTriangles img1 tringle1 avgTriangles
     writeImage "toCari.jpg"  $ warpTriangles img1 tringle1 $ interpTriangles 1.2 avgTriangles tringle1
     writeImage "toCari2.jpg"  $ warpTriangles img1 tringle1 $ interpTriangles 1.5 avgTriangles tringle1
     writeImage "toCari3.jpg"  $ warpTriangles img1 tringle1 $ interpTriangles 1.7 avgTriangles tringle1
     writeImage "toCari4.jpg"  $ warpTriangles img1 tringle1 $ interpTriangles 2 avgTriangles tringle1
     writeImage "fromAverage.jpg"  $ warpTriangles avgFace avgTriangles tringle1
     writeImage "average.jpg" avgFace
     putStrLn "done"


