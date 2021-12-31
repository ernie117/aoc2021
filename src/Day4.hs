module Day4 where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

type BingoCard = [[Maybe Int]]

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
getInput = fmap lines (readFile "input/test4.txt")

bingoNumbers :: [String] -> [Int]
bingoNumbers = map read . splitOn "," . head

getGame :: [String] -> Game
getGame ss = Game (bingoNumbers ss) (makePlayers (tail $ tail ss))

makePlayers :: [String] -> [Player]
makePlayers ss = map makePlayer (splitOn [""] ss)

makePlayer :: [String] -> Player
makePlayer ss =
  Player 0 (map (map (Just . read) . filter (not . null) . splitOn " ") ss)

remove :: Int -> [Maybe Int] -> [Maybe Int]
remove x e = result
  where
    present = elemIndex (Just x) e
    result =
      case present of
        Nothing -> e
        (Just i) -> one ++ [Nothing] ++ tail two
          where (one, two) = splitAt i e

mark :: Int -> [Maybe Int] -> [Maybe Int]
mark x r =
  if Just x `elem` r
    then remove x r
    else r

markCard' :: Int -> Player -> Player
markCard' x g = Player x (map (mark x) (bingoCard g))

markCards' :: Int -> [Player] -> [Player]
markCards' x = map (markCard' x)

scorePlayer :: Player -> Int
scorePlayer g = lastNumber g * sum' g
  where
    sum' p = sum $ map (sum . catMaybes) (bingoCard p)

draw :: Int -> Game -> Game
draw i g = Game (draws g) (markCards' i (players g))

-- playGame :: Game -> Game
-- playGame g = Game dss completedPlayers
--   where
--     dss = draws g
--     completedPlayers = takeWhile (isWonG) (map (markCard' dss) (players g))
-- solve' :: Game -> Int
-- solve' = scorePlayer . head . filter isWonG . players . playGame
checkRows :: BingoCard -> Bool
checkRows = (not . null) . concat . filter (all isNothing)

isWonG :: Player -> Bool
isWonG g = checkRows (bingoCard g) || checkRows (transpose (bingoCard g))

-- Part I solution: 87456
-- Part II solution: 15561
main :: IO ()
main
  -- getInput >>= print . solve . mapResults . numsAndCards
 = do
  getInput >>=
    print .
    scorePlayer .
    head .
    filter isWonG .
    players .
    draw 24 .
    draw 21 .
    draw 14 .
    draw 0 .
    draw 2 .
    draw 23 . draw 17 . draw 11 . draw 5 . draw 9 . draw 4 . draw 7 . getGame
