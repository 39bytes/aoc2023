module Main where

import Data.Array
import Data.List qualified as L
import Data.Tuple (swap)

type Grid = Array Int (Array Int Char)

type Point = (Int, Int)

toGrid :: [String] -> Grid
toGrid ls = array (0, height - 1) $ zip [0 ..] $ map createRow ls
  where
    width = length $ head ls
    height = length ls
    createRow l = array (0, width - 1) $ zip [0 ..] l

findGalaxies :: Grid -> [Point]
findGalaxies grid = [(i, j) | i <- [0 .. length grid - 1], j <- [0 .. length (grid ! 0) - 1], grid ! i ! j == '#']

expandRows :: [String] -> [Point] -> Int -> [Point]
expandRows ls pts amount = foldr step pts emptyRows
  where
    emptyRows = [i | (i, r) <- zip [0 ..] ls, all (== '.') r]
    step row = map (\(y, x) -> if y >= row then (y + amount, x) else (y, x))

expand :: [String] -> [Point] -> Int -> [Point]
expand ls pts amount = map swap pts''
  where
    pts' = expandRows ls pts amount
    pts'' = expandRows (L.transpose ls) (map swap pts') amount

pairDists :: [Point] -> Int
pairDists galaxies = sum dists `div` 2
  where
    dists = [abs (y1 - y2) + abs (x1 - x2) | p1@(y1, x1) <- galaxies, p2@(y2, x2) <- galaxies, p1 /= p2]

solve1 :: [String] -> Int
solve1 ls = pairDists pts
  where
    grid = toGrid ls
    galaxies = findGalaxies grid
    pts = expand ls galaxies 1

solve2 :: [String] -> Int
solve2 ls = pairDists pts
  where
    grid = toGrid ls
    galaxies = findGalaxies grid
    pts = expand ls galaxies 999999

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  print $ solve1 ls
  print $ solve2 ls
