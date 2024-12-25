module Day25 (day25ex, day25a) where

import Data.List (transpose)
import Data.List.Split (splitOn)

splitInput :: [Char] -> [[Char]]
splitInput = splitOn "\n\n"

isKey :: String -> Bool
isKey = all (== '#') . last . lines

isLock :: String -> Bool
isLock = all (== '#') . head . lines

locks :: [String] -> [[String]]
locks = map lines . filter isLock

keys :: [String] -> [[String]]
keys = map lines . filter isKey

reach :: [Char] -> Int
reach = length . filter (== '#')

allReaches :: [[Char]] -> [Int]
allReaches = map reach . transpose

sizeLock :: String -> Int
sizeLock = length . lines

fits :: [[Char]] -> [[Char]] -> Bool
fits lock key = all (<= length lock) $ zipWith (+) (allReaches lock) (allReaches key)

allPossibleCombos :: [Char] -> [([String], [String])]
allPossibleCombos input = [(x, y) | x <- allLocks, y <- allKeys, fits x y]
  where
    allLocks = locks parts
    allKeys = keys parts
    parts = splitInput input

solve25 :: FilePath -> IO ()
solve25 filename = do
  contents <- readFile filename
  print $ length $ allPossibleCombos contents

day25ex :: IO ()
day25ex = solve25 "input/day25example.txt"

day25a :: IO ()
day25a = solve25 "input/day25.txt"
