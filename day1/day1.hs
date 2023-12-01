module Main where

import Data.Char (isDigit)
import Data.List (find, isPrefixOf, isSuffixOf)

digitLine :: String -> Int
digitLine s = read [firstDigit s, firstDigit (reverse s)]
  where
    firstDigit s = head $ filter isDigit s

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitLine2 :: String -> Int
digitLine2 s = read [firstDigit s (zip digits ['1' .. '9']), firstDigit (reverse s) (zip (map reverse digits) ['1' .. '9'])]
  where
    firstDigit s ds = case find ((`isPrefixOf` s) . fst) ds of
      Just (_, n) -> n
      Nothing -> let c = head s in if isDigit c then c else firstDigit (tail s) ds

solve :: (String -> Int) -> [String] -> Int
solve f ss = sum $ map f ss

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  print (solve digitLine ls)
  print (solve digitLine2 ls)
