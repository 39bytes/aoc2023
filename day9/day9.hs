module Main where

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diff :: [Int] -> [Int]
diff [] = []
diff [x] = []
diff (x : xs) = head xs - x : diff xs

diffs :: [Int] -> [[Int]]
diffs = takeWhile (any (/= 0)) . iterate diff

extrapolate :: [Int] -> Int
extrapolate xs = go $ diffs xs
  where
    go [s] = head s
    go (s : ss) = last s + go ss

solve1 :: [[Int]] -> Int
solve1 hists = sum $ map extrapolate hists

extrapolateBackwards :: [Int] -> Int
extrapolateBackwards xs = go $ diffs xs
  where
    go [s] = head s
    go (s : ss) = head s - go ss

solve2 :: [[Int]] -> Int
solve2 hists = sum $ map extrapolateBackwards hists

main :: IO ()
main = do
  inp <- getContents
  let hists = parse inp
  print $ solve1 hists
  print $ solve2 hists
