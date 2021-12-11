module Day2 where

-- First part:
-- forward X increases the horizontal position by X units.
-- down X increases the depth by X units.
-- up X decreases the depth by X units.
-- Second part:
-- down X increases your aim by X units.
-- up X decreases your aim by X units.
-- forward X does two things:
--     It increases your horizontal position by X units.
--     It increases your depth by your aim multiplied by X.
getInput :: IO [(String, Int)]
getInput =
  fmap
    (map ((\x -> (head x, read (last x))) . words) . lines)
    (readFile "input/input-day2.txt")

increment :: [(String, Int)] -> Int
increment = go (0, 0)
  where
    go (x, y) [] = x * y
    go (hor, dep) (z:zs)
      | direction == "forward" = go (hor + delta, dep) zs
      | direction == "down" = go (hor, dep + delta) zs
      | direction == "up" = go (hor, dep - delta) zs
      where
        direction = fst z
        delta = snd z

aimIncrement :: [(String, Int)] -> Int
aimIncrement = go (0, 0, 0)
  where
    go (x, y, _) [] = x * y
    go (hor, dep, aim) (z:zs)
      | direction == "forward" = go (hor + delta, dep + (aim * delta), aim) zs
      | direction == "down" = go (hor, dep, aim + delta) zs
      | direction == "up" = go (hor, dep, aim - delta) zs
      where
        direction = fst z
        delta = snd z

solution1 :: [(String, Int)] -> Int
solution1 = increment

solution2 :: [(String, Int)] -> Int
solution2 = aimIncrement

-- Part 1 answer = 1451208
main :: IO ()
main = do
  getInput >>= print . solution1
  getInput >>= print . solution2
