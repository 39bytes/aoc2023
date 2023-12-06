module Main where

import Data.List.Split (splitOn)

parse :: [String] -> ([String], [String])
parse (l1 : l2 : _) = (splitLine l1, splitLine l2)
  where
    splitLine l = let [_, right] = splitOn ":" l in words right

ways :: Int -> Int -> Int
ways time record = length $ filter (> record) $ map (\t -> t * (time - t)) [0 .. time]

solve1 :: [String] -> [String] -> Int
solve1 times dists = product $ zipWith ways ts ds
  where
    ts = map read times
    ds = map read dists

solve2 :: [String] -> [String] -> Int
solve2 times dists = ways time record
  where
    time = read $ concat times
    record = read $ concat dists

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let (times, dists) = parse ls
  print $ solve1 times dists
  print $ solve2 times dists
