module Main where

import Data.List
import Data.String
import System.Random
import System.Environment
import System.Exit
import Text.Read
import Codec.Picture

import Lib

readMint :: String -> Int
readMint str = case (readMaybe (str) :: Maybe Int) of
    Nothing -> -1
    Just a -> a

readMfloat :: String -> Float
readMfloat str = case (readMaybe (str) :: Maybe Float) of
    Nothing -> -1.0
    Just a -> a

main = do
    args <- getArgs
    case (length args) of
        3 -> putStr ""
        _ -> exitWith (ExitFailure 84)
     

    let n = readMint (args !! 0)
    let prec = readMfloat (args !! 1)
    case ((n <= 0) || (prec <= 0)) of
        True -> exitWith (ExitFailure 84)
        False -> putStr ""
    picture <- readFile (args !! 2)
    case ((length picture) == 0) of
        True -> exitWith (ExitFailure 84)
        False -> do
            seed  <- newStdGen
            printClusters (clusterize (n) (prec) (readTXT picture) (createRandomClusters n (randomlist (n*3) seed)) [])

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

printList :: [Int] -> IO ()
printList [] = putStr ""
printList (x:list) = do
    putStr "("
    putStr (show (mod x 255))
    putStrLn ")" 
    printList list

-- Functions to launch if you want to open a BMP (Some Function have to get fixed in lib.hs)

-- picture <- readImage (args !! 2)
--case picture of
--    Left err -> do 
--        putStrLn ("Could not read image: " ++ err)
--        exitWith (ExitFailure 84)
--    Right (ImageRGB8 img) -> do
--        printClusters (clusterize (n) (prec) (imageMapping img 0 0))
--    Right _ -> do
--        putStrLn "Unexpected pixel format"
--        exitWith (ExitFailure 84)
-------imageMapping :: Image PixelRGB8 -> Int -> Int -> [MyPixel]
-------imageMapping img w h = case ((w + 1) >= (imageWidth img)) of
-------    False -> ([(inPixel img w h)] ++ (imageMapping img (w+1) h))
-------    True -> case ((h + 1) >= (imageHeight img)) of
-------        True -> [(inPixel img ((imageWidth img) - 1) ((imageHeight img)-1))]
-------        False -> ([(inPixel img w h)] ++ (imageMapping img 0 (h+1)))
-------    
-------inPixel :: Image PixelRGB8 -> Int -> Int -> MyPixel
-------inPixel img w h = MyPixel { position = (w, h), color = (pixelAt img w h)}
