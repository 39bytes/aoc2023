module Main where

import Data.List qualified as L
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromJust)

type Directions = String

type Network = M.Map String (String, String)

parse :: [String] -> (Directions, Network)
parse ls = (directions, M.fromList $ map parseNode maps)
  where
    (directions : _ : maps) = ls
    parseNode l =
      let [name, lr] = splitOn " = " l
       in let [left, right] = splitOn ", " lr
           in (name, (tail left, init right))

adj :: (Ord k) => k -> M.Map k v -> v
adj k m = fromJust $ M.lookup k m

pick :: Char -> (a, a) -> a
pick 'L' = fst
pick 'R' = snd

lastIs :: Char -> String -> Bool
lastIs c = (== c) . last

dfs :: (String -> Bool) -> Directions -> Network -> Int -> String -> Int
dfs done _ _ acc cur | done cur = acc
dfs done (d : ds) network acc cur = dfs done ds network (acc + 1) next
  where
    next = pick d $ adj cur network

solve1 :: Directions -> Network -> Int
solve1 dirs network = dfs (== "ZZZ") (cycle dirs) network 0 "AAA"

solve2 :: Directions -> Network -> Int
solve2 dirs network = foldl lcm 1 (map getLen startNodes)
  where
    getLen = dfs (lastIs 'Z') (cycle dirs) network 0
    startNodes = filter (lastIs 'A') $ M.keys network

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let (dirs, network) = parse ls
  print $ length dirs
  print $ solve1 dirs network
  print $ solve2 dirs network
