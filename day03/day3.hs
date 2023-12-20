module Main where

import Control.Monad (mfilter)
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Map qualified as M

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) (x : _) i | i == 0 = Just x
(!?) (_ : xs) i = xs !? (i - 1)

dirs8 :: [(Int, Int)]
dirs8 = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

adjSymbols :: [String] -> Int -> Int -> [(Char, (Int, Int))]
adjSymbols grid i j = [t | Just t@(c, _) <- adjChars, c /= '.', not $ isDigit c]
  where
    adj = map (\(y, x) -> (y + i, x + j)) dirs8
    adjChars =
      map
        ( \(y, x) -> do
            l <- grid !? y
            c <- l !? x
            Just (c, (y, x))
        )
        adj

enumerate2d :: [[a]] -> [[(a, (Int, Int))]]
enumerate2d xss = zipWith zip xss [[(i, j) | j <- [0 ..]] | i <- [0 ..]]

revRead :: String -> Int
revRead digits = read (reverse digits)

some :: [a] -> Bool
some = not . null

solve1 :: [String] -> Int
solve1 grid = sum $ concatMap partNums (enumerate2d grid)
  where
    step (digits, symbolAdj, acc) (c, (i, j))
      | isDigit c = (c : digits, symbolAdj || some (adjSymbols grid i j), acc)
      | symbolAdj && some digits = ("", False, revRead digits : acc)
      | otherwise = ("", False, acc)
    partNums row = case foldl step ("", False, []) row of
      (digits, True, nums) -> revRead digits : nums
      (_, _, nums) -> nums

solve2 :: [String] -> Int
solve2 grid = sum $ map product $ filter ((== 2) . length) $ M.elems allMap
  where
    adjGears grid i j = [point | (c, point) <- adjSymbols grid i j, c == '*']
    gearMaps gears digits = map (\t -> M.singleton t [revRead digits]) gears
    (+++) m ms = M.unionsWith (++) (m : ms)

    step (digits, gears, acc) (c, (i, j))
      | isDigit c = (c : digits, gears `L.union` adjGears grid i j, acc)
      | some digits = ("", [], acc +++ gearMaps gears digits)
      | otherwise = ("", [], acc)
    gearNums row = case foldl step ("", [], M.empty) row of
      ("", _, acc) -> acc
      (digits, gears, acc) -> acc +++ gearMaps gears digits
    allMap = M.unionsWith (++) $ map gearNums (enumerate2d grid)

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  print (solve1 ls)
  print (solve2 ls)
