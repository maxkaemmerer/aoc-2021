import qualified Data.Text    as Text
import qualified Data.Text.IO as IoText
import Data.Maybe
import Data.List

incrementIfLarger :: (Int, Int) -> Int -> (Int, Int)
incrementIfLarger (total, previous) current
    | current > previous = (total + 1, current) 
    | otherwise = (total, current)  

countIncreases :: [Int] -> (Int, Int)
countIncreases measurements = foldl incrementIfLarger (0, maximum measurements) measurements

stringsToInts :: [String] -> [Int]
stringsToInts = map read


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile


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

mainPartOne :: IO()
mainPartOne = do
    measurements <- readLines "./measurements.txt" -- should be 1475
    -- measurements <- readLines "./example.txt" -- should be 7
    let measurementsAsInts = stringsToInts measurements
    print $ fst $ countIncreases measurementsAsInts

mainPartTwo :: IO()
mainPartTwo = do
    measurements <- readLines "./measurements.txt" -- should be 1516
    -- measurements <- readLines "./example.txt" -- should be 5
    let measurementsAsInts = stringsToInts measurements
    print $ fst $ countIncreases $ mapToThreeMeasures measurementsAsInts