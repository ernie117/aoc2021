module Day4 where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

type Row = [Maybe Int]

type BingoCard = [Row]

data Game =
  Game
    { draws :: [Int]
    , players :: [Player]
    }
  deriving (Show, Eq)

data Player =
  Player
    { lastNumber :: Int
    , bingoCard :: BingoCard
    }
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

mark :: Int -> Row -> Row
mark x = map (mfilter (/= x))

markCards' :: Int -> [Player] -> [Player]
markCards' x g = map (markCard' x) g
  where
    markCard' i g = Player x (map (mark x) (bingoCard g))

scorePlayer :: Player -> Int
scorePlayer g = lastNumber g * sum' g
  where
    sum' p = sum $ concatMap catMaybes (bingoCard p)

drawNumber :: Game -> Game
drawNumber g = Game (tail dss) (markCards' (head dss) (players g))
  where
    dss = draws g

checkRows :: BingoCard -> Bool
checkRows = (not . null) . concat . filter (all isNothing)

gameIsWon :: Game -> Bool
gameIsWon g = any isWon (players g)

isWon :: Player -> Bool
isWon p = checkRows (bingoCard p) || checkRows (transpose (bingoCard p))

getWinner :: Game -> Player
getWinner = fromJust . find isWon . players

solve1 :: Game -> Int
solve1 = scorePlayer . getWinner . until gameIsWon drawNumber

-- Test input solution: 4512
-- Part I solution: 87456
-- Part II solution: 15561
main :: IO ()
main = do
  getInput >>= print . solve1 . getGame
