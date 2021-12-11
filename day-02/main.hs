import Text.Read
import Data.Maybe

data Instruction = Forward Int
                | Down Int
                | Up Int
                | None
                deriving (Show, Eq)

type State = (Int, Int)

parseInstruction :: String -> Maybe Instruction
parseInstruction s = case words s of
    ["down", y] -> Down <$> readMaybe y
    ["forward", y] -> Forward <$> readMaybe y
    ["up", y] ->  Up <$> readMaybe y
    _ -> Nothing

parseInstructions :: [String] -> [Instruction]
parseInstructions = mapMaybe parseInstruction

applyInstruction :: State -> Instruction -> State
applyInstruction (depth, hpos) (Down distance) = (depth + distance, hpos)
applyInstruction (depth, hpos) (Forward distance) = (depth, hpos + distance)
applyInstruction (depth, hpos) (Up distance) = (depth - distance, hpos)

navigate :: State -> [Instruction] -> State
navigate = foldl applyInstruction

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

calculateScore :: State -> Int
calculateScore (depth, hpos) = depth * hpos

main :: IO()
main = do
    -- rawInstructions <- readLines "./example.txt" -- should be 150
    rawInstructions <- readLines "./instructions.txt" -- should be 1484118
    print $ calculateScore $ navigate (0, 0) $ parseInstructions rawInstructions