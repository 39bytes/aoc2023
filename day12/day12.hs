module Main where

import Data.List qualified as L
import Data.List.Split (splitOn)
import Debug.Trace (trace)

type Record = (String, [Int])

parse :: [String] -> [Record]
parse = map parseRecord
  where
    parseRecord l =
      let [springs, groupLengths] = words l
       in (springs, map read $ splitOn "," groupLengths)

unfoldRecord :: Record -> Record
unfoldRecord (springs, groups) = (springs', groups')
  where
    springs' = L.intercalate "?" $ replicate 5 springs
    groups' = concat $ replicate 5 groups

debug = flip trace

arrangements :: Record -> Int
arrangements (springs, groups) = dfs springs groups

dfs :: String -> [Int] -> Int
dfs "" [] = 1
dfs cs [] | '#' `notElem` cs = 1
dfs cs gs | null cs || null gs = 0
dfs (c : cs) groups@(g : gs) = case c of
  '?' -> pickPath '.' + pickPath '#'
  c -> pickPath c
  where
    (group, rest) = splitAt (g - 1) cs
    validGroup = '.' `notElem` group && length group == g - 1
    pickPath c = case c of
      '.' -> dfs cs groups
      '#' | validGroup && null rest -> dfs rest gs
      '#' | validGroup && head rest /= '#' -> dfs (tail rest) gs
      '#' -> 0

solve :: [Record] -> Int
solve rs = sum $ map arrangements rs

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let records = parse ls
  print $ solve records
