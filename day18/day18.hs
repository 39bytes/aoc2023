module Main where

import Data.Char (digitToInt, toUpper)
import Data.List qualified as L

type Instruction = (Char, Int, String)

type Point = (Int, Int)

type Direction = (Int, Int)

parse :: [String] -> [Instruction]
parse = map parseInstruction
  where
    parseInstruction l = let [dir, num, color] = words l in (head dir, read num, tail (init color))

fromDir :: Char -> Direction
fromDir 'R' = (1, 0)
fromDir 'L' = (-1, 0)
fromDir 'U' = (0, -1)
fromDir 'D' = (0, 1)

intToDir :: Char -> Char
intToDir '0' = 'R'
intToDir '1' = 'D'
intToDir '2' = 'L'
intToDir '3' = 'U'

shoelace :: [Point] -> Int
shoelace points = abs (pos - neg) `div` 2
  where
    pos = sum $ zipWith (*) (map snd points) (map fst (tail points))
    neg = sum $ zipWith (*) (map fst points) (map snd (tail points))

pAdd :: Point -> Point -> Point
pAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pMul :: Point -> Int -> Point
pMul (x, y) c = (c * x, c * y)

pSub :: Point -> Point -> Point
pSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

pLen :: Point -> Int
pLen (x, y) = abs $ x + y

getVertices :: [(Int, Int)] -> [Point]
getVertices vecs = go vecs (0, 0) [] ++ [(0, 0)]
  where
    go [] _ acc = acc
    go (v : vecs) cur acc = go vecs next (next : acc)
      where
        next = cur `pAdd` v

getDirVecs :: [Instruction] -> [(Int, Int)]
getDirVecs = map (\(dir, amt, _) -> fromDir dir `pMul` amt)

getDirVecs2 :: [Instruction] -> [(Int, Int)]
getDirVecs2 = map (\(_, _, '#' : hex) -> fromDir (intToDir $ last hex) `pMul` hexToInt (init hex))

hexToInt :: String -> Int
hexToInt s = sum $ zipWith (*) (iterate (* 16) 1) $ reverse (map digitToInt s)

perimeter :: [Point] -> Int
perimeter pts = sum $ zipWith (\a b -> pLen $ b `pSub` a) pts (tail pts)

solve :: [Point] -> Int
solve vertices = i + b
  where
    area = shoelace vertices
    b = perimeter vertices
    i = area - (b `div` 2) + 1

main :: IO ()
main = do
  inp <- getContents
  let instructions = parse (lines inp)
  let vs = getVertices (getDirVecs instructions)
  print $ solve vs
  let vs2 = getVertices (getDirVecs2 instructions)
  print $ solve vs2
