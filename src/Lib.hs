module Lib
    (
      printPixels,
      printClusters,
      createRandomClusters,
      clusterize,
      findMinimum,
      readTXT,
      allMin,
      averageCentroid,
      nearestCentroid,
      MyPixel(..),
      MyCluster(..),
      ColorPoint(..)
    ) where

import Data.List
import Data.String
import Data.Word
import System.Environment
import System.Exit
import System.Random
import Text.Printf
import Codec.Picture

data MyPixel = MyPixel
    { position :: (Int, Int)
    , color :: (Int, Int, Int)
    }

data MyCluster = MyCluster
    { centroid :: ColorPoint,
      pixels :: [MyPixel]
    }

data ColorPoint = ColorPoint
    { r :: Float,
      g :: Float,
      b :: Float
    }

--
-- For a txt
--

readTXT :: String -> [MyPixel]
readTXT s = map mappingPixel (removeEmptyLine (lines s))

removeEmptyLine :: [String] -> [String]
removeEmptyLine arr = case (hasEmptyLine arr) of
        True -> delete "" arr
        False -> arr

hasEmptyLine :: [String] -> Bool
hasEmptyLine []     = False
hasEmptyLine (x:list) = case (null x) of
        True -> True
        False -> hasEmptyLine list

mappingPixel :: String -> MyPixel
mappingPixel l = MyPixel { position = (read ((words (map commaSupr noparpos)) !! 0) :: Int,
                                       read ((words (map commaSupr noparpos)) !! 1) :: Int),
                          color   =  ((read ((words (map commaSupr noparcol)) !! 0) :: Int),
                                      (read ((words (map commaSupr noparcol)) !! 1) :: Int),
                                      (read ((words (map commaSupr noparcol)) !! 2) :: Int)) }
                        where
                            noparpos = (((words l) !! 0) \\ "()")
                            noparcol = (((words l) !! 1) \\ "()")


commaSupr :: Char -> Char
commaSupr ',' = ' '
commaSupr c   = c    -- picture <- readImage (args !! 2)


printPixels :: [MyPixel] -> IO()
printPixels [] = putStr ""
printPixels (x:list) = do
    printPixel x
    printPixels list

printPixel :: MyPixel -> IO()
printPixel (MyPixel {position = (x, y), color = (r, g, b)}) = do
    printf "(%d,%d) (%d,%d,%d)\n" x y r g b

printColorPoint :: ColorPoint -> IO()
printColorPoint (ColorPoint r g b) = do
    putStrLn "--"
    putStr "("
    putStr (show r')
    putStr ","
    putStr (show g')
    putStr ","
    putStr (show b')
    putStrLn ")"
    putStrLn "-"
    where
        r' = (round r)
        g' = (round g)
        b' = (round b)

printClusters :: [MyCluster] -> IO()
printClusters [] = putStr ""
printClusters (x:list) = do
    printCluster x
    printClusters list

printCluster :: MyCluster -> IO()
printCluster (MyCluster centroid pixels) = do
    printColorPoint centroid
    printPixels pixels

printTEST :: IO()
printTEST = do
    putStrLn "TEST"

--
-- The Following Function are usefull for the k-mean partition
--

pixelDistance ::  (Int, Int, Int) -> ColorPoint -> Float
pixelDistance (ra, ga, ba) (ColorPoint rb gb bb) = sqrt(((fromIntegral ra :: Float) - (rb)) * ((fromIntegral ra :: Float) - (rb)) +
                                                        ((fromIntegral ga :: Float) - (gb)) * ((fromIntegral ga :: Float) - (gb)) +
                                                        ((fromIntegral ba :: Float) - (bb)) * ((fromIntegral ba :: Float) - (bb)))

pixelDistanceCP ::  ColorPoint -> ColorPoint -> Float
pixelDistanceCP (ColorPoint ra ga ba) (ColorPoint rb gb bb) = sqrt(((ra) - (rb)) * ((ra) - (rb)) +
                                                                   ((ga) - (gb)) * ((ga) - (gb)) +
                                                                   ((ba) - (bb)) * ((ba) - (bb)))

averageCentroids :: [MyCluster] -> [MyCluster]
averageCentroids [] = []
averageCentroids (x:list) = [averageCentroid x] ++ averageCentroids list

averageCentroid :: MyCluster -> MyCluster
averageCentroid (MyCluster cent []) = MyCluster { centroid = cent, pixels = []}
averageCentroid (MyCluster cent pix) = MyCluster { centroid = ColorPoint red green blue, pixels = []}
    where
        red   = divfloat (averageFromTuple 0 pix) (length pix)
        green = divfloat (averageFromTuple 1 pix) (length pix)
        blue  = divfloat (averageFromTuple 2 pix) (length pix)

divfloat :: Int -> Int -> Float
divfloat a b = (fromIntegral a) / (fromIntegral b)

averageFromTuple :: Int -> [MyPixel] -> Int
averageFromTuple n [] = 0
averageFromTuple n (x:pixels) =  ((toList (color x)) !! n) + averageFromTuple n pixels

toList :: (Int, Int, Int) -> [Int]
toList (r, g, b) = [r] ++ [g] ++ [b]

clusterize :: Int -> Float -> [MyPixel] -> [MyCluster] -> [MyCluster] -> [MyCluster]
clusterize n prec pixels newclus oldclus = case (finishLimit (length newclus) newclus oldclus prec) of
    True  -> thing
    False -> clusterize n prec pixels (averageCentroids thing) thing
    where
        thing = (nearestCentroid pixels newclus)

finishLimit :: Int -> [MyCluster] -> [MyCluster] -> Float -> Bool
finishLimit 1 _ _ _ = True
finishLimit _ _ [] _ = False
finishLimit n newclus oldclus prec = ((pixelDistanceCP nouv old) < prec) && (finishLimit (n-1) newclus oldclus prec)
    where
        nouv = (centroid (newclus !! (n-1)))
        old  = (centroid (oldclus !! (n-1)))

randomNbCP :: Int -> [Int] -> ColorPoint
randomNbCP n rands = ColorPoint r g b
        where
            r = fromIntegral (mod (rands !! ((div (length rands) 3) - n    )) 255) :: Float
            g = fromIntegral (mod (rands !! ((div (length rands) 3) - n + 1)) 255) :: Float
            b = fromIntegral (mod (rands !! ((div (length rands) 3) - n + 2)) 255) :: Float


createRandomClusters :: Int -> [Int] -> [MyCluster]
createRandomClusters 0 rands = []
createRandomClusters n rands = ([MyCluster { centroid = randomNbCP n rands, pixels = []}] ++ createRandomClusters (n - 1) rands)

nearestCentroid :: [MyPixel] -> [MyCluster] -> [MyCluster]
nearestCentroid [] clusters = clusters
nearestCentroid (x:pixel) clusters = nearestCentroid pixel (findMinAndAddToCluster x clusters)

findMinAndAddToCluster :: MyPixel -> [MyCluster] -> [MyCluster]
findMinAndAddToCluster pixel clusters = replaceNth a new clusters
    where
        new = MyCluster { centroid = (centroid (clusters !! a)), pixels = (pixels (clusters !! a)) ++ [pixel] }
        a = (findMinimum (allMin pixel (length clusters) clusters))

replaceNth :: Int -> MyCluster -> [MyCluster] -> [MyCluster]
replaceNth n a list = addNth n 0 a (removeNth n 0 list)

removeNth :: Int -> Int -> [MyCluster] -> [MyCluster]
removeNth n m [] = []
removeNth n m (x:list) = case (n == m) of
    True -> list
    False -> [x] ++ removeNth n (m+1) list

addNth :: Int -> Int -> MyCluster -> [MyCluster] -> [MyCluster]
addNth n m a [] = [a]
addNth n m a (x:list) = case (n == m) of
    True -> [x] ++ [a] ++ list
    False -> [x] ++ addNth n (m+1) a list

allMin :: MyPixel -> Int -> [MyCluster] -> [Float]
allMin p 0 clusters = []
allMin p n clusters =  (allMin p (n-1) clusters) ++ [minimalDist p (clusters !! (n-1))]

minimalDist :: MyPixel -> MyCluster -> Float
minimalDist pix (MyCluster cp pixels) = pixelDistance (color pix) (ColorPoint (r cp) (g cp) (b cp))

findMinimum :: [Float] -> Int
findMinimum x = case elemIndex (minimum x) x of
                Nothing -> -1
                Just n  -> n
