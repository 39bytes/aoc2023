module Main where

import Data.Char (isDigit)
import Data.Map qualified as M

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : b) -> a : splitOn c b
  (a, "") -> [a]

parseGame :: String -> [(String, Int)]
parseGame s = map (makePair . words) $ splitOn ',' s
  where
    makePair [n, color] = (color, read n)

parseSets :: String -> (Int, [[(String, Int)]])
parseSets s = (gameId, map parseGame sets)
  where
    (before, ':' : ' ' : game) = break (== ':') s
    gameId :: Int = read $ dropWhile (not . isDigit) before
    sets = splitOn ';' game

cubeNums :: M.Map String Int
cubeNums = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

gamePossible :: String -> Int
gamePossible s = if all (all allowed) sets then gameId else 0
  where
    (gameId, sets) = parseSets s
    allowed (c, n) = case M.lookup c cubeNums of
      Just num -> n <= num
      Nothing -> False

cubePower :: [[(String, Int)]] -> Int
cubePower = product . M.elems . minCubes
  where
    minCubes cs = foldl (\acc (c, n) -> M.insertWith max c n acc) M.empty (concat cs)

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  print (sum $ map gamePossible ls)
  print (sum $ map (cubePower . snd . parseSets) ls)
