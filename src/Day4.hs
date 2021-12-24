module Day4 where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

allAlphas :: String -> Bool
allAlphas s = all (`elem` s) ['a' .. 'z']

type BingoCard = [[Int]]

getInput :: IO [String]
getInput = fmap lines (readFile "input/input-day4.txt")

bingoNumbers :: [String] -> [Int]
bingoNumbers = map read . splitString (== ',') . head

splitString :: (Char -> Bool) -> String -> [String]
splitString f s =
  if snd broken == ""
    then [fst broken]
    else fst broken : splitString f (snd broken)
  where
    broken = break f (dropWhile f s)

makeCard :: [String] -> BingoCard
makeCard = map row
  where
    row = map read . splitString isSpace

numsAndCards :: [String] -> ([Int], [BingoCard])
numsAndCards ss = (bingoNumbers ss, makeCards (tail $ tail ss))

makeCards :: [String] -> [BingoCard]
makeCards ls = map makeCard (separate ls)

separate :: [String] -> [[String]]
separate [] = []
separate [x] = [[x]]
separate xs = fst spanned : separate (rest $ snd spanned)
  where
    spanned = span (/= "") xs
    rest [] = []
    rest rs = tail $ snd spanned

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

markCard :: [Int] -> BingoCard -> Result
markCard x b = go x b 0 0
  where
    go l c wn count
      | checkRows c || checkRows (transpose c) =
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
