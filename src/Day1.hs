module Day1 where

getInput :: IO [Int]
getInput = fmap (map read . lines) (readFile "input/input-day1.txt")

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:zs)
  | y > x = 1 + countIncreases (y : zs)
  | otherwise = countIncreases (y : zs)

triple :: [Int] -> [Int]
triple [] = []
triple [x] = []
triple [x, y] = []
triple (x:y:z:rest) = x + y + z : triple (y : z : rest)

solution1 :: [Int] -> Int
solution1 = countIncreases

solution2 :: [Int] -> Int
solution2 = countIncreases . triple

-- Part 1 answer: 1722
-- Part 2 answer: 1748
main :: IO ()
main = do
  getInput >>= print . countIncreases
  getInput >>= print . solution2
