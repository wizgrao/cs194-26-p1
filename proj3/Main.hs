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
     img  <- readImg $ head args :: IO Im
     img2  <- readImg $ args !! 1 :: IO  Im
     displayImage img
     displayImage img2  
     eitherCSV1 <- parseCSVFromFile $ args !! 2
     print eitherCSV1
     let csv1 = cheeseError eitherCSV1
     eitherCSV2 <- parseCSVFromFile $ args !! 3
     let csv2 = cheeseError eitherCSV2
     if null csv1 || null csv2 then putStrLn "Error Loading CSV" else putStrLn "csvs loaded!"
     let tringle1 = csvToTriangles csv1
     let tringle2 = csvToTriangles csv2
     displayImage $ warpTriangles img tringle1 tringle2
     if args !! 4 /= "yuh" then mapM_ (\i -> writeImage (args !! 4 ++ show i ++ ".png") (interpImages (1.0/45.0*fromIntegral i) img tringle1 img2 tringle2) >> putStrLn ("Wrote " ++ show i)) [0 .. 44] else return ()
     void getLine  


