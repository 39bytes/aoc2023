module Main where

import Data.Array
import Data.List qualified as L
import Data.Sequence (ViewL ((:<)), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Debug.Trace (trace)

debug = flip trace

type Point = (Int, Int)

type Direction = (Int, Int)

type Grid = Array Point Char

type Queue a = Seq.Seq a

makeGrid :: [String] -> Grid
makeGrid ls = array ((0, 0), (width - 1, height - 1)) elems
  where
    width = length $ head ls
    height = length ls
    indices = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    elems = zip indices $ concat ls

at :: Grid -> Point -> Maybe Char
at grid (x, y)
  | x < minX || x > maxX || y < minY || y > maxY = Nothing
  | otherwise = Just $ grid ! (x, y)
  where
    ((minX, minY), (maxX, maxY)) = bounds grid

pAdd :: Point -> Point -> Point
pAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

up, down, right, left :: Direction
up = (0, -1)
down = (0, 1)
right = (1, 0)
left = (-1, 0)

reflect :: Char -> Direction -> Direction
reflect '\\' (0, -1) = (-1, 0)
reflect '\\' (1, 0) = (0, 1)
reflect '\\' (0, 1) = (1, 0)
reflect '\\' (-1, 0) = (0, -1)
reflect '/' (0, -1) = (1, 0)
reflect '/' (1, 0) = (0, -1)
reflect '/' (0, 1) = (-1, 0)
reflect '/' (-1, 0) = (0, 1)

follow :: Grid -> Queue (Point, Direction) -> S.Set (Point, Direction) -> S.Set (Point, Direction)
follow grid q visit = case Seq.viewl q of
  Seq.EmptyL -> visit
  pd :< rest | pd `elem` visit -> follow grid rest visit
  pd@(pos, dir) :< rest -> follow grid nextQ nextVisit
    where
      (|>>) q dir = q |> (pos `pAdd` dir, dir)
      nextQ = case grid `at` pos of
        Just c -> case c of
          c | c == '\\' || c == '/' -> rest |>> reflect c dir
          '-' | dir == up || dir == down -> rest |>> left |>> right
          '|' | dir == left || dir == right -> rest |>> up |>> down
          _ -> rest |>> dir
        Nothing -> rest
      nextVisit = case grid `at` pos of
        Just _ -> S.insert pd visit
        Nothing -> visit

uniquePoints :: S.Set (Point, Direction) -> Int
uniquePoints visit = length $ L.nub $ map fst $ S.toList visit

energized grid dir start = uniquePoints $ follow grid (Seq.singleton (start, dir)) S.empty

solve :: Grid -> Int
solve grid = energized grid (1, 0) (0, 0)

solve2 :: Grid -> Int
solve2 grid = maximum $ concat [l, r, u, d]
  where
    (_, (w, h)) = bounds grid
    followFrom dir p = follow grid (Seq.singleton (p, dir)) S.empty
    l = map (energized grid (1, 0)) $ zip (repeat 0) [0 .. h]
    r = map (energized grid (-1, 0)) $ zip (repeat w) [0 .. h]
    u =
      map (energized grid (0, 1)) $
        zip [0 .. w] (repeat 0)
    d = map (energized grid (0, -1)) $ zip [0 .. w] (repeat h)

main :: IO ()
main = do
  inp <- getContents
  let grid = makeGrid $ lines inp
  print $ solve grid
  print $ solve2 grid
