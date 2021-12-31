module Day4 where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

type BingoCard = [[Int]]

getInput :: IO [String]
getInput = fmap lines (readFile "input/input-day4.txt")

bingoNumbers :: [String] -> [Int]
bingoNumbers = map read . splitOn "," . head

makeCard :: [String] -> BingoCard
makeCard = map row
  where
    row = map read . filter (not . null) . splitOn " "

numsAndCards :: [String] -> ([Int], [BingoCard])
numsAndCards ss = (bingoNumbers ss, makeCards (tail $ tail ss))

makeCards :: [String] -> [BingoCard]
makeCards ls = map makeCard (splitOn [""] ls)

checkRows :: BingoCard -> Bool
checkRows = (not . null) . concat . filter (all (== -1))

replace :: Int -> [Int] -> [Int]
replace x e = one ++ [-1] ++ tail two
  where
    (one, two) = splitAt (fromJust (elemIndex x e)) e

data Result =
  Result
    { winningNumber :: Int
    , count :: Int
    , bingoCard :: BingoCard
    }
  deriving (Show)

mapResults :: ([Int], [BingoCard]) -> [Result]
mapResults (xs, bs) = map (markCard xs) bs

-- something like:
-- until checkStuff markCard card

isWon :: BingoCard -> Bool
isWon c = checkRows c || checkRows (transpose c)

markCard :: [Int] -> BingoCard -> Result
markCard x b = go x b 0 0
  where
    go l c wn count
      | isWon c =
        Result {winningNumber = wn, count = count, bingoCard = c}
      | null l && (checkRows c || checkRows (transpose c)) =
        Result {winningNumber = 0, count = count, bingoCard = c}
      | null l = Result {winningNumber = 0, count = count, bingoCard = c}
      | otherwise =
        go
          (tail l)
          (map
             (\e ->
                if head l `elem` e
                  then replace (head l) e
                  else e)
             c)
          (head l)
          (succ count)

solve :: [Result] -> Int
solve xs = winningNumber winner * sumCard (bingoCard winner)
  where
    winner = maximumBy (compare `on` count) xs

sumCard :: BingoCard -> Int
sumCard b = sum $ map sum filtered
  where
    filtered = map (filter (/= (-1))) b

-- Part I solution: 87456
-- Part II solution: 15561
main :: IO ()
main = do
  getInput >>= print . solve . mapResults . numsAndCards
  -- input <- getInput
  -- let stuff = makeCards (tail input)
  -- print stuff
