module Day5 where

import Data.List
import Data.List.Split

type Dimensions = (Int, Int)

type Range = [Dimensions]

getInput :: IO [String]
getInput = fmap lines (readFile "input/input-day5.txt")

getCoords :: [String] -> Range
getCoords =
  concatMap expand . straights . map (map pair . filter (/= "->") . words)

getFullCoords :: [String] -> Range
getFullCoords =
  concatMap expand . map (map pair . filter (/= "->") . words)

pair :: String -> Dimensions
pair = go . splitOn ","
  where
    go [x, y] = (read x, read y)

straights :: [Range] -> [Range]
straights = filter (\[(x1, y1), (x2, y2)] -> x1 == x2 || y1 == y2)

expand :: Range -> Range
expand ((x1, y1):(x2, y2):_) = zip (compare' x1 x2) (compare' y1 y2)
  where
    compare' w z
      | w < z = [w .. z]
      | w > z = [w,(pred w) .. z]
      | otherwise = repeat w

solve1 :: Range -> Int
solve1 = length . filter (>= 2) . map length . group . sort

-- Test data solution: 5
-- Part I solution: 7414
-- Part II solution: 19676
main :: IO ()
main = do
  getInput >>= print . solve1 . getCoords
  getInput >>= print . solve1 . getFullCoords
