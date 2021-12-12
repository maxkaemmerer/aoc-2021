import Data.List
import Data.Maybe
import Text.Read

data Bit = Zero
    | One
    deriving (Show, Eq)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

intToBit :: Int -> Maybe Bit
intToBit i = case i of
    1 -> Just One
    0 -> Just Zero
    otherwise -> Nothing

charToMaybeInt :: Char -> Maybe Int
charToMaybeInt = (readMaybe :: String -> Maybe Int) . charToString

mapStringToBits :: String -> [Bit]
mapStringToBits = mapMaybe intToBit . mapMaybe charToMaybeInt

charToString :: Char -> String
charToString = (:[])

listOfBitsToDecimal :: Int -> [Bit] -> Int
listOfBitsToDecimal position bits = case bits of 
    [] -> 0
    xs -> currentBitAsDecimal + listOfBitsToDecimal nextPosition allButLastBit
    where
        allButLastBit = init bits
        lastBit = last bits
        currentBitAsDecimal = (2 ^ position) * bitToInt lastBit
        nextPosition = position + 1

bitToInt :: Bit -> Int
bitToInt bit = case bit of
    One -> 1
    Zero -> 0

mostCommonBit :: [Bit] -> Bit
mostCommonBit bits
    | length ones >= length zeroes = One
    | otherwise = Zero
    where
        ones = filter (== One) bits
        zeroes = filter (== Zero) bits

leastCommonBit :: [Bit] -> Bit
leastCommonBit bits
    | length ones < length zeroes = One
    | otherwise = Zero
    where
        ones = filter (== One) bits
        zeroes = filter (== Zero) bits

calculateGammaRate :: [[Bit]] -> Int
calculateGammaRate bits = (listOfBitsToDecimal 0) $ map mostCommonBit $ transpose bits

calculateEpsilonRate :: [[Bit]] -> Int
calculateEpsilonRate bits = (listOfBitsToDecimal 0) $ map leastCommonBit $ transpose bits

calculateOxygenGeneratorRating :: [[Bit]] -> Int
calculateOxygenGeneratorRating bits = (listOfBitsToDecimal 0) $ breakDownListOfBitsToMostCommon bits

calculateCO2ScrubberRating :: [[Bit]] -> Int
calculateCO2ScrubberRating bits = (listOfBitsToDecimal 0) $ breakDownListOfBitsToLeastCommon bits

breakDownListOfBitsToMostCommon :: [[Bit]] -> [Bit]
breakDownListOfBitsToMostCommon bits = case bits of
    [x] -> x
    (x:y:xs) -> mostCommon : breakDownListOfBitsToMostCommon remainingBits
    where
        mostCommon = mostCommonBit $ head $ transpose bits
        remainingBits = map tail $ filter (\measurement -> mostCommon == head measurement) bits

breakDownListOfBitsToLeastCommon :: [[Bit]] -> [Bit]
breakDownListOfBitsToLeastCommon bits = case bits of
    [x] -> x
    (x:y:xs) -> leastCommon : breakDownListOfBitsToLeastCommon remainingBits
    where
        leastCommon = leastCommonBit $ head $ transpose bits
        remainingBits = map tail $ filter (\measurement -> leastCommon == head measurement) bits

mainPartOne :: IO()
mainPartOne = do
    -- rawDiagnostics <- readLines "./example.txt" -- should be 198
    rawDiagnostics <- readLines "./diagnostics.txt" -- should be 2261546
    let bits = map mapStringToBits rawDiagnostics
    print $ calculateGammaRate bits * calculateEpsilonRate bits

mainPartTwo :: IO()
mainPartTwo = do
    -- rawDiagnostics <- readLines "./example.txt" -- should be 230
    rawDiagnostics <- readLines "./diagnostics.txt" -- should be 6775520
    let bits = map mapStringToBits rawDiagnostics
    print $ calculateOxygenGeneratorRating bits * calculateCO2ScrubberRating bits
             