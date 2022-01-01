module Day5 where

import Data.Char
import Data.Foldable (toList)
import Data.Function
import Data.List hiding (intersperse)
import Data.List.Split
import Data.Sequence (Seq, (><), filter, fromList, index, intersperse, update)

type Dimensions = (Int, Int)

type Range = [Dimensions]

type Sequences = Seq (Seq Char)

data Matrix =
  Matrix Sequences Range

instance Show Matrix where
  show (Matrix sq _) = toList $ foldr1 (><) (intersperse (fromList "\n") sq)

pair :: String -> (Int, Int)
pair = go . splitOn ","
  where
    go [x, y] = (read x, read y)

getInput :: IO [String]
getInput = fmap lines (readFile "input/input-day5.txt")

getCoords :: [String] -> Range
getCoords = concatMap (expand . map pair . Prelude.filter (/= "->") . words)

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

incLine :: Dimensions -> Sequences -> Sequences
incLine (x, y) sqs = update x (increment y line) sqs
  where
    line = index sqs x
    increment idx sqss
      | element == '.' = update idx '1' sqss
      | otherwise = update idx (intToDigit (succ (digitToInt element))) sqss
      where
        element = index sqss idx

updateMatrix :: Matrix -> Matrix
updateMatrix (Matrix sqs (r:rs)) = Matrix (incLine r sqs) rs

dimensions :: Range -> Dimensions
dimensions rs = (maxX rs, maxY rs)
  where
    maxX = fst . maximumBy (compare `on` fst)
    maxY = snd . maximumBy (compare `on` snd)

buildMatrix :: Range -> Matrix
buildMatrix rs =
  Matrix
    (fromList (replicate (snd ds + 1) $ fromList (replicate (fst ds + 1) '.')))
    rs
  where
    ds = dimensions rs

rangeEmpty :: Matrix -> Bool
rangeEmpty (Matrix _ rs) = null rs

countOverlaps :: Matrix -> Int
countOverlaps (Matrix sqs _) =
  length $ Data.Sequence.filter (>= '2') $ foldr1 (><) sqs

solve1 :: Matrix -> Int
solve1 = countOverlaps . until rangeEmpty updateMatrix

-- Test data solution: 5
-- Part I solution: 7414
-- Part II solution: _
main :: IO ()
main = do
  getInput >>= print . solve1 . buildMatrix . getCoords
