module Day8 where

import Data.List.Split

-- 1 = 2
-- 4 = 4
-- 7 = 3
-- 8 = 7
getInput :: IO [String]
getInput = lines <$> readFile "input/input-day8.txt"

getSigsAndOutputs :: [String] -> [[String]]
getSigsAndOutputs = map (splitOn "|")

getOutputs :: [[String]] -> Int
getOutputs ss = sum $ map getUniques splits
  where
    splits = map (words . (!! 1)) ss

getUniques :: [String] -> Int
getUniques ss =
  length $ filter (\l -> l == 2 || l == 4 || l == 3 || l == 7) lengths
  where
    lengths = map length ss

main :: IO ()
main = do
  getInput >>= print . getOutputs . getSigsAndOutputs
