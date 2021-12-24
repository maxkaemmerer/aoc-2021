module Util (readLines, tailWithEmpty, splitString, safeHead) where

import Data.List

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

safeHead :: [a] -> Maybe a
safeHead list = case list of
    [] -> Nothing
    (x:xs) -> Just x