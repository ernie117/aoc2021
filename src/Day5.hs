module Day5 where

import Data.Char
import Data.Foldable (toList)
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Sequence as SQ
import Data.Sequence ((><))

type Dimensions = (Int, Int)

type Range = [Dimensions]

type Sequences = SQ.Seq (SQ.Seq Char)

pair :: String -> (Int, Int)
pair = go . splitOn ","
  where
    go [x, y] = (read x, read y)

getInput :: IO [String]
getInput = fmap lines (readFile "input/input-day5.txt")

getCoords :: [String] -> Range
getCoords = concatMap (expand . map pair . filter (/= "->") . words)

expand :: Range -> Range
expand r
  | isDiagonal  = []
  | ascendingX  = fillAscX
  | descendingX = fillDescX
  | ascendingY  = fillAscY
  | descendingY = fillDescY
  | otherwise   = r
  where
    (x1, y1, x2, y2)             = unpack r
    unpack ((x1, y1):(x2, y2):_) = (x1, y1, x2, y2)
    isDiagonal                   = x1 /= x2 && y1 /= y2
    ascendingX                   = x1 < x2
    descendingX                  = x1 > x2
    ascendingY                   = y1 < y2
    descendingY                  = y1 > y2
    fillAscX                     = [(x, y1) | x <- [x1 .. x2]]
    fillDescX                    = [(x, y1) | x <- [x1,(pred x1) .. x2]]
    fillAscY                     = [(x1, y) | y <- [y1 .. y2]]
    fillDescY                    = [(x1, y) | y <- [y1,(pred y1) .. y2]]

solve1 :: Range -> Int
solve1 = length . filter (>= 2) . map length . group . sort

-- Test data solution: 5
-- Part I solution: 7414
-- Part II solution: _
main :: IO ()
main = do
  getInput >>= print . solve1 . getCoords
