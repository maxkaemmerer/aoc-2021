import qualified Data.Text    as Text
import qualified Data.Text.IO as IoText

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

main :: IO()
main = do
    measurements <- readLines "./measurements.txt" -- should be 1475
    -- measurements <- readLines "./example.txt" -- should be 7
    let measurementsAsInts = stringsToInts measurements
    print $ fst $ countIncreases measurementsAsInts