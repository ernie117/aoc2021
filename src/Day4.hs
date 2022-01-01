module Day4 where

import Control.Monad (mfilter)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe

type Row = [Maybe Int]

type BingoCard = [Row]

data Game =
  Game [Int] [Player]
  deriving (Show, Eq)

data Player =
  Player Int BingoCard
  deriving (Show, Eq)

getInput :: IO [String]
getInput = fmap lines (readFile "input/input-day4.txt")

bingoNumbers :: [String] -> [Int]
bingoNumbers = map read . splitOn "," . head

getGame :: [String] -> Game
getGame ss = Game (bingoNumbers ss) (makePlayers (tail $ tail ss))

makePlayers :: [String] -> [Player]
makePlayers ss = map makePlayer (splitOn [""] ss)
  where
    makePlayer ls = Player 0 (map (map (Just . read) . words) ls)

markCards :: Int -> [Player] -> [Player]
markCards x = map (markCard x)
  where
    markCard n (Player _ ps) = Player x (map (map (mfilter (/= n))) ps)

scorePlayer :: Player -> Int
scorePlayer (Player n card) = n * sum' card
  where
    sum' = sum . concatMap catMaybes

drawNumber :: Game -> Game
drawNumber (Game (d:ds) ps) = Game ds (markCards d ps)

checkRows :: BingoCard -> Bool
checkRows = (not . null) . concat . filter (all isNothing)

gameIsWon :: Game -> Bool
gameIsWon (Game _ ps) = any winningPlayer ps

winningPlayer :: Player -> Bool
winningPlayer (Player _ card) = checkRows card || checkRows (transpose card)

getWinner :: Game -> Player
getWinner (Game _ ps) = fromJust $ find winningPlayer ps

solve1 :: Game -> Int
solve1 = scorePlayer . getWinner . until gameIsWon drawNumber

-- Test input solution: 4512
-- Part I solution: 87456
-- Part II solution: 15561
main :: IO ()
main = do
  getInput >>= print . solve1 . getGame
