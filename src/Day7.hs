module Day7 where

import Data.Function
import Data.List
import Data.List.Split

getInput :: IO [Int]
getInput = map read . splitOn "," <$> readFile "input/input-day7.txt"

mapNumbers :: [Int] -> Int
mapNumbers xs = snd $ minimumBy (compare `on` snd) $ (map doSubtract xs)
  where
    doSubtract i = (i, sum $ map (\x -> abs (i - x)) xs)

-- Very slow; found this faster calculation: (diff^2 + abs diff) `div` 2
mapNumbers2 :: [Int] -> Int
mapNumbers2 xs = snd $ minimumBy (compare `on` snd) $ (map doSubtract xs)
  where
    doSubtract i = (i, sum $ map (\x -> sum [1..(abs (i - x))]) xs)

-- Part I solution: 337488
-- Part II solution: 89647695
main :: IO ()
main = do
  getInput >>= print . mapNumbers
  getInput >>= print . mapNumbers2
