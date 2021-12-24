module Day02 (mainPartOne, mainPartTwo) where

import Text.Read
import Data.Maybe
import qualified Util

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

calculateScore :: State -> Int
calculateScore (depth, hpos) = depth * hpos

type StateWithAim = (Int, Int, Int)

applyInstructionWithAim :: StateWithAim -> Instruction -> StateWithAim
applyInstructionWithAim (depth, hpos, aim) (Down distance) = (depth, hpos, aim + distance)
applyInstructionWithAim (depth, hpos, aim) (Forward distance) = (depth + (aim * distance), hpos + distance, aim)
applyInstructionWithAim (depth, hpos, aim) (Up distance) = (depth, hpos, aim - distance)

navigateWithAim :: StateWithAim -> [Instruction] -> StateWithAim
navigateWithAim = foldl applyInstructionWithAim

calculateScoreWithAim :: StateWithAim -> Int
calculateScoreWithAim (depth, hpos, _) = depth * hpos

mainPartOne :: String -> IO Int
mainPartOne file = do
    rawInstructions <- Util.readLines file
    pure $ calculateScore $ navigate (0, 0) $ parseInstructions rawInstructions

mainPartTwo :: String -> IO Int
mainPartTwo file = do
    rawInstructions <- Util.readLines file
    pure $ calculateScoreWithAim $ navigateWithAim (0, 0, 0) $ parseInstructions rawInstructions