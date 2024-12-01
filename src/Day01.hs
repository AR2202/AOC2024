module Day01
  ( readInput,
    day1sol,
    day2sol,
  )
where

import Data.List (sort)

readInt :: String -> Int
-- Part 1
readInt = read

readInput :: IO ([Int], [Int])
readInput = do
  input <- readFile "input/day01.txt"
  let inputNums = (map . map) readInt $ map words $ lines input
  let list1 = map head inputNums
  let list2 = map last inputNums
  let list1sorted = sort list1
  let list2sorted = sort list2
  return (list1sorted, list2sorted)

day1sol :: IO ()
day1sol = do
  (list1sorted, list2sorted) <- readInput
  let diffs = zipWith (-) list1sorted list2sorted
  let res = sum $ map abs diffs
  print res

-- Part 2

ocurrances :: Eq a => a -> [a] -> Int
ocurrances element list = (length . filter (== element)) list

day2sol :: IO ()
day2sol = do
  (list1sorted, list2sorted) <- readInput
  let numOcurrances = map (flip ocurrances list2sorted) list1sorted
  let res = sum $ zipWith (*) numOcurrances list1sorted
  print res
