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
    | length ones > length zeroes = One
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

mainPartOne :: IO()
mainPartOne = do
    -- rawDiagnostics <- readLines "./example.txt" -- should be 198
    rawDiagnostics <- readLines "./diagnostics.txt" -- should be 2261546
    let bits = map mapStringToBits rawDiagnostics
    print ((calculateGammaRate bits ) * (calculateEpsilonRate bits))