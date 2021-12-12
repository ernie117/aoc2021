module Day3 where

import Data.Char (digitToInt, intToDigit)
import Data.Function
import Data.List

getInput :: IO [[Int]]
getInput = fmap (map (map digitToInt) . lines) (readFile "input/input-day3.txt")

getInput2 :: IO [String]
getInput2 = lines <$> readFile "input/input-day3.txt"

flipBits :: [Int] -> [Int]
flipBits = map (`subtract` 1)

bin2int :: [Int] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0 . reverse

process :: [[Int]] -> [Int]
process =
  map
    ((\[zeros, ones] ->
        if length zeros > length ones
          then 0
          else 1) .
     group . sort) .
  transpose

calculatePower :: [Int] -> Int
calculatePower xs = gamma * epsilon
  where
    gamma = bin2int xs
    epsilon = bin2int $ flipBits xs

getBinaryNumber :: [String] -> ([Int] -> Int) -> String
getBinaryNumber = go 0
  where
    go _ [s] _ = s
    go idx ls g = go (succ idx) (filter (\l -> l !! idx == mostCommon) ls) g
      where
        mostCommon = intToDigit $ mostCommonAtIndex g idx ls

mostCommon :: [Int] -> Int
mostCommon xs
  | 2 * sum xs >= length xs = 1
  | otherwise = 0

leastCommon :: [Int] -> Int
leastCommon xs
  | 2 * sum xs < length xs = 1
  | otherwise = 0

mostCommonAtIndex :: ([Int] -> Int) -> Int -> [String] -> Int
mostCommonAtIndex g idx = g . map digitToInt . flip (!!) idx . transpose

solution1 :: [[Int]] -> Int
solution1 = calculatePower . process

solution2 :: [String] -> Int
solution2 ls = oxygen * co2
  where
    oxygen = bin2int $ map digitToInt $ getBinaryNumber ls mostCommon
    co2 = bin2int $ map digitToInt $ getBinaryNumber ls leastCommon

-- Part 1 answer = 3885894
-- Part 1 answer = 3775 * 1159 == 4375225
main :: IO ()
main = do
  getInput >>= print . solution1
  getInput2 >>= print . solution2
