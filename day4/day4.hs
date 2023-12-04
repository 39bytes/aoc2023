module Main where

import Data.Array qualified as A
import Data.List qualified as L
import Data.List.Split (splitOn)

parseCard :: String -> ([Int], [Int])
parseCard s = (winningSet, playSet)
  where
    [_, card] = splitOn ":" s
    [left, right] = splitOn "|" card
    nums s = map read $ words s
    winningSet = nums left
    playSet = nums right

winningNums :: ([Int], [Int]) -> Int
winningNums (xs, ys) = length $ xs `L.intersect` ys

parseWinningNums :: String -> Int
parseWinningNums = winningNums . parseCard

solve1 :: [String] -> Int
solve1 ss = sum $ map points ss
  where
    points card = let k = parseWinningNums card in if k > 0 then 2 ^ (k - 1) else 0

solve2 :: [String] -> Int
solve2 ss = go cardNums (replicate (length ss) 1) 0
  where
    cardNums = map parseWinningNums ss
    go [] _ total = total
    go (x : xs) (c : counts) acc =
      let (incr, next) = splitAt x counts
       in go xs (map (+ c) incr ++ next) (acc + c)

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  print (solve1 ls)
  print (solve2 ls)
