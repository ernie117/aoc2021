module Day6 where

import qualified Data.HashMap.Lazy as Map
import Data.List
import Data.List.Split

type Timer = Int

type Freq = Int

type LanternMap = Map.HashMap Timer Freq

getInput :: IO [Int]
getInput = map read . splitOn "," <$> readFile "input/input-day6.txt"

makeMap :: [Int] -> LanternMap
makeMap ls = Map.fromList fromInput
  where
    fromInput = map (\x -> (head x, length x)) $ group $ sort ls

recycle :: LanternMap -> LanternMap
recycle m =
  Map.delete (-1) $ Map.insertWith (+) 6 (Map.findWithDefault 0 (-1) m) m

births :: LanternMap -> LanternMap
births m = Map.insert 8 (Map.findWithDefault 0 (-1) m) m

age :: LanternMap -> LanternMap
age = Map.mapKeys (subtract 1)

day :: LanternMap -> LanternMap
day = recycle . births . age

solve1 :: LanternMap -> Int
solve1 m = Map.foldr (+) 0 $ iterate day m !! 80

solve2 :: LanternMap -> Int
solve2 m = Map.foldr (+) 0 $ iterate day m !! 256

-- Part I solution: 388739
-- Part II solution: 1741362314973
main :: IO ()
main = do
  getInput >>= print . solve1 . makeMap
  getInput >>= print . solve2 . makeMap
