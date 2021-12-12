import Data.List
import Data.Maybe
import Text.Read
import qualified Data.Text as Text (pack, unpack, replace)
import  Distribution.Simple.Utils (safeHead)


type Field = (Int, Bool)
type Board = [[Field]]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

tailWithEmpty :: [a] -> [a]
tailWithEmpty list = case list of
    [] -> []
    (x:xs) -> xs

splitString :: Char -> String -> [String]
splitString delimiter string = case string of
    [] -> []
    xs -> takeWhile (/= delimiter) xs : (splitString delimiter $ tailWithEmpty $ dropWhile (/=delimiter) xs)

inputIntoBoards :: [String] -> [[String]]
inputIntoBoards string = case string of
    [] -> []
    xs -> takeWhile (/="") xs : (inputIntoBoards $ tailWithEmpty $ dropWhile (/="") xs)


replaceInString :: String -> String -> String -> String
replaceInString needle replacement haystack = 
    Text.unpack $ Text.replace (Text.pack needle) (Text.pack replacement) (Text.pack haystack)

strip :: String -> String
strip string =
    reverse $ dropWhile (==' ') $ reverse $ dropWhile (==' ') string


applyTurnToBoard :: Int -> Board -> Board
applyTurnToBoard number = map (map $ markFieldIfValid number)


markFieldIfValid :: Int -> Field -> Field
markFieldIfValid number field
    | number == (fst field) = ((number, True) :: Field)
    | otherwise = field

intToField :: Int -> Field
intToField number = ((number, False) :: Field)


stringsToInts :: [String] -> [Int]
stringsToInts =  (mapMaybe readMaybe :: [String] -> [Int])

stringToFields :: String -> [Field]
stringToFields = (map intToField) . stringsToInts . splitString ' ' . strip . replaceInString "  " " "

parseBoards :: [String] -> [Board]
parseBoards lines = map (map stringToFields) $ filter (/=[]) $ inputIntoBoards $ tail lines

findVictoriousBoard :: [Board] -> Maybe Board
findVictoriousBoard boards = safeHead $ filter isVictorious boards

findNonVictoriousBoard :: [Board] -> [Board]
findNonVictoriousBoard boards = filter (not . isVictorious) boards

isVictorious :: Board -> Bool
isVictorious board = or [(any (all snd) board), (any (all snd) (transpose board))]

applyTurnsUntilVictory :: [Int] -> [Board] -> (Int, Board)
applyTurnsUntilVictory turns boards =
    fromMaybe (applyTurnsUntilVictory (tail turns) newsBoards) (fmap (\board -> (currentTurn, board)) victoriousBoard)
    where
        currentTurn = head turns
        newsBoards = map (applyTurnToBoard currentTurn) boards
        victoriousBoard = findVictoriousBoard newsBoards

applyTurnsUntilOneBoardIsLeft :: [Int] -> [Board] -> (Int, Board)
applyTurnsUntilOneBoardIsLeft turns boards = case boards of
    [x] -> (currentTurn, applyTurnToBoard currentTurn x)
    xs ->applyTurnsUntilOneBoardIsLeft (tail turns) (findNonVictoriousBoard newsBoards)
    where
        currentTurn = head turns
        newsBoards = map (applyTurnToBoard currentTurn) boards

calculateScore :: (Int, Board) -> Int
calculateScore (lastNumber, board) = lastNumber * boardSum
    where
        boardSum = foldl (\currentTotal row -> currentTotal + (sum $ map fieldToScore row)) 0 board

fieldToScore :: Field -> Int
fieldToScore (number, marked) = if marked then 0 else number

mainPartOne :: IO()
mainPartOne = do
    -- rawInput <- readLines "./example.txt" -- should be 4512
    rawInput <- readLines "./boards.txt" -- should be 10374
    let turns = (mapMaybe readMaybe :: [String] -> [Int]) $ splitString ',' $ head rawInput
    let boards = parseBoards rawInput
    print $ calculateScore $ applyTurnsUntilVictory turns boards

mainPartTwo :: IO()
mainPartTwo = do
    -- rawInput <- readLines "./example.txt" -- should be 1924
    rawInput <- readLines "./boards.txt" -- should be ?
    let turns = (mapMaybe readMaybe :: [String] -> [Int]) $ splitString ',' $ head rawInput
    let boards = parseBoards rawInput
    print $ calculateScore $ applyTurnsUntilOneBoardIsLeft turns boards