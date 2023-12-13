module Main where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

type Record = (String, [Int])

parse :: [String] -> [Record]
parse = map parseRecord
  where
    parseRecord l =
      let [springs, groupLengths] = words l
       in (springs, map read $ splitOn "," groupLengths)

debug = flip trace

arrangements :: Record -> Int
arrangements (springs, groups) = dfs springs groups 0

-- Brute force
dfs :: String -> [Int] -> Int -> Int
dfs "" [] _ = 1
dfs "" [g] groupCount | g == groupCount = 1
dfs cs [] _ | all (`elem` ".?") cs = 1
dfs cs gs _ | null cs || null gs = 0
dfs (c : cs) groups@(g : gs) groupCount = case c of
  '?' -> pickPath '.' + pickPath '#'
  c -> pickPath c
  where
    pickPath c = case c of
      '.' | groupCount == g -> dfs cs gs 0
      '.' | groupCount == 0 -> dfs cs groups 0
      '.' -> 0
      '#' | groupCount >= g -> 0
      '#' -> dfs cs groups (groupCount + 1)

solve1 :: [Record] -> Int
solve1 rs = sum $ map arrangements rs

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let records = parse ls
  print $ solve1 records
