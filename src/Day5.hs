module Day5 where

import Data.Char
import Data.Foldable (toList)
import Data.Function
import Data.List hiding (intersperse)
import Data.List.Split
import Data.Sequence (fromList, update, Seq, (><), intersperse)

type Dimensions = (Int, Int)
type Range = [Dimensions]
type Sequences = Seq (Seq Char)
newtype Matrix = Matrix Sequences

instance Show Matrix where
  show (Matrix sq) = toList $ foldr1 (><) (intersperse (fromList "\n") sq)

pair :: String -> (Int,Int)
pair = go . splitOn ","
  where
    go [x,y] = (read x, read y)

getInput :: IO [String]
getInput = fmap lines (readFile "input/test5.txt")

getCoords :: [String] -> [Range]
getCoords = map (map pair . filter (/= "->") . words)

expand :: Range -> Range
expand r
  | isDiagonal  = []
  | ascendingX  = fillAscX
  | descendingX = fillDescX
  | ascendingY  = fillAscY
  | descendingY = fillDescY
  | otherwise   = r
    where
      (x1, y1, x2, y2)           = unpack r
      unpack ((x1,y1):(x2,y2):_) = (x1, y1, x2, y2)
      isDiagonal                 = x1 /= x2 && y1 /= y2
      ascendingX                 = x1 < x2
      descendingX                = x1 > x2
      ascendingY                 = y1 < y2
      descendingY                = y1 > y2
      fillAscX                   = [(x,y1) | x <- [x1..x2]]
      fillDescX                  = [(x,y1) | x <- [x1,(pred x1)..x2]]
      fillAscY                   = [(x1,y) | y <- [y1..y2]]
      fillDescY                  = [(x1,y) | y <- [y1,(pred y1)..y2]]

-- incLine :: Dimensions -> [Seq Char] -> [Seq Char]
-- incLine d sqs = update y line '1'
--   where
--     (x, y) = d
--     line = sqs !! x

-- TODO `until range empty updateMatrix` or something
updateMatrix :: Matrix -> Range -> Matrix
updateMatrix (Matrix sqs) (r:rs) = undefined

dimensions :: [Range] -> Dimensions
dimensions rs = (maxX flat, maxY flat)
  where
    maxX = fst . maximumBy (compare `on` fst)
    maxY = snd . maximumBy (compare `on` snd)
    flat = concat rs

buildMatrix :: Dimensions -> Matrix
buildMatrix ds = Matrix (fromList (replicate (snd ds + 1) $ fromList (replicate (fst ds + 1) '.')))

main :: IO ()
main = do
  getInput >>= mapM_ print . filter (not . null) . map expand . getCoords
  getInput >>= print . buildMatrix . dimensions . getCoords
