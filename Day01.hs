module Day01 (mainPartOne, mainPartTwo) where

import qualified Data.Text    as Text
import qualified Data.Text.IO as IoText
import Data.Maybe
import Data.List
import qualified Util

incrementIfLarger :: (Int, Int) -> Int -> (Int, Int)
incrementIfLarger (total, previous) current
    | current > previous = (total + 1, current) 
    | otherwise = (total, current)  

countIncreases :: [Int] -> (Int, Int)
countIncreases measurements = foldl incrementIfLarger (0, maximum measurements) measurements

stringsToInts :: [String] -> [Int]
stringsToInts = map read

sumFirstThree :: [Int] -> Maybe Int
sumFirstThree list
    | length firstThree == 3 = Just $ sum firstThree
    | otherwise = Nothing
    where firstThree = take 3 list

mapToThreeMeasures :: [Int] -> [Int]
mapToThreeMeasures measurements = mapMaybe sumFirstThree $ tails measurements

 -- whoops this is apparently a thing already... https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html#v:tails
tails' :: [Int] -> [[Int]]
tails' list = foldl (\rests val -> rests ++ [(tail $ last rests)]) [list] list

mainPartOne :: String -> IO Int
mainPartOne file = do
    measurements <- Util.readLines file
    let measurementsAsInts = stringsToInts measurements
    pure $ fst $ countIncreases measurementsAsInts

mainPartTwo :: String ->  IO Int
mainPartTwo file = do
    measurements <- Util.readLines file
    let measurementsAsInts = stringsToInts measurements
    pure $ fst $ countIncreases $ mapToThreeMeasures measurementsAsInts