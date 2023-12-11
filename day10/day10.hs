module Main where

import Control.Monad (msum)
import Data.Array
import Data.Bifunctor (bimap, second)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Monoid (First)
import Debug.Trace (trace)

type Grid = Array Int (Array Int Char)

type Point = (Int, Int)

data Direction = North | South | West | East deriving (Enum, Show)

parse :: [String] -> Grid
parse ls = array (0, height - 1) $ zip [0 ..] $ map createRow ls
  where
    width = length $ head ls
    height = length ls
    createRow l = array (0, width - 1) $ zip [0 ..] l

findStart :: Grid -> Point
findStart grid = go $ assocs grid
  where
    go ((i, row) : rs) = case L.find ((== 'S') . snd) $ assocs row of
      Just (j, _) -> (i, j)
      Nothing -> go rs

connected :: Char -> Char -> Direction -> Bool
connected 'S' _ _ = True
connected _ 'S' _ = True
connected a b North = a `elem` ['|', 'L', 'J'] && b `elem` ['7', 'F', '|']
connected a b South = connected b a North
connected a b West = a `elem` ['-', 'J', '7'] && b `elem` ['-', 'L', 'F']
connected a b East = connected b a West

withinBounds :: Grid -> Point -> Bool
withinBounds grid (i, j) = 0 <= i && i <= h && 0 <= j && j <= w
  where
    (_, h) = bounds grid
    (_, w) = bounds $ head $ elems grid

debug = flip trace

dirs :: [Point]
dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]

dfs :: Grid -> Point -> Point -> Int -> Maybe Int
dfs grid (i, j) (pi, pj) acc | grid ! i ! j == 'S' && (i /= pi || j /= pj) = Just acc
dfs grid p@(i, j) prev acc = case moveable of
  [] -> Nothing
  _ -> msum $ map (\adj -> dfs grid adj p (acc + 1)) moveable
  where
    neighbors = zipWith (curry $ second $ bimap (+ i) (+ j)) [North ..] dirs
    at p = grid ! fst p ! snd p
    canMove dir to = to /= prev && withinBounds grid to && connected (at p) (at to) dir
    moveable = map snd $ filter (uncurry canMove) neighbors

solve1 :: Grid -> Int
solve1 grid = loopLength `div` 2
  where
    start = findStart grid
    loopLength = fromJust $ dfs grid start start 0

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let grid = parse ls
  print $ solve1 grid
