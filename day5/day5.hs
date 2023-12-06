module Main where

import Criterion (bench, nf)
import Criterion.Main (defaultMain)
import Data.Char (isDigit)
import Data.List qualified as L
import Data.List.Split (chunksOf, splitWhen)

type RangeMap = (Int, Int, Int)

type Input = ([Int], [[RangeMap]])

parseGroup :: [String] -> [RangeMap]
parseGroup [] = []
parseGroup (g : gs) = let [a, b, c] = nums g in (a, b, c) : parseGroup gs
  where
    nums g = map read (words g)

groups :: [String] -> [[String]]
groups = map (drop 1) . splitWhen null

parseInput :: [String] -> Input
parseInput ls = (seeds, map parseGroup $ groups $ drop 2 ls)
  where
    seeds = map read $ words $ dropWhile (not . isDigit) $ head ls

minMap :: (a -> Int) -> [a] -> Int
minMap f xs = minimum $ map f xs

followable :: [RangeMap] -> Int -> Int -> [(Int, Int)]
followable [] _ _ = []
followable ((dst, src, len) : gs) x l
  | x <= src && src < x + l = (dst, min (x + l - src) len) : followable gs x l
  | src <= x && x < src + len = (dst + x - src, min (src + len - x) l) : followable gs x l
  | otherwise = followable gs x l

dfs :: [[RangeMap]] -> Int -> Int -> Int
dfs [] x l = x
dfs groups x l =
  case groups of
    [_] | null follow -> x
    [_] -> min x $ minMap fst follow
    (_ : gs) | null follow -> dfs gs x l
    (_ : gs) -> minMap (uncurry $ dfs gs) follow
  where
    follow = followable (head groups) x l

solve1 :: Input -> Int
solve1 (seeds, maps) = minMap (\x -> dfs maps x 1) seeds

solve2 :: Input -> Int
solve2 (seeds, maps) = minMap (\[x, l] -> dfs maps x l) $ chunksOf 2 seeds

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let input = parseInput ls
  print $ solve1 input
  print $ solve2 input
  defaultMain
    [ bench "part 1: " $ nf solve1 input,
      bench "part 2: " $ nf solve2 input
    ]
