module Day02
  ( day2asol,
    day2bsol,
  )
where

import Data.List (init, tails)

readInt :: String -> Int
readInt = read

-- | read Day2 input
readInp :: FilePath -> IO [[Int]]
readInp fname = do
  contents <- readFile fname
  let reports = lines contents
  let levels = map (map readInt . words) reports
  return levels

-- Part 1
isMonotonic :: Ord b => [b] -> Bool

-- | checks if list is monotonic
isMonotonic list =
  isMonotonicIncreasing list
    || isMonotonicDecreasing list
  where
    isMonotonicIncreasing l =
      and $
        zipWith (<) l (tail l)
    isMonotonicDecreasing l =
      and $
        zipWith (>) l (tail l)

diffBelow4 :: (Ord a, Num a) => [a] -> Bool
diffBelow4 l = all ((<= 3) . abs) $zipWith (-) l (tail l)

safeReps :: (Ord a, Num a) => [[a]] -> [[a]]
safeReps =
  filter
    ( \x ->
        isMonotonic x && diffBelow4 x
    )

numSafeReps :: [[Int]] -> Int
numSafeReps = length . safeReps

-- | filter reports that are safe
day2asol :: IO ()
day2asol = do
  reports <- readInp "input/day02.txt"
  let sol = numSafeReps reports
  print sol

-- Part 2
unsafeReports :: (Ord a, Num a) => [[a]] -> [[a]]
unsafeReports =
  filter
    ( not
        . ( \x ->
              isMonotonic x && diffBelow4 x
          )
    )

isSafeWithOneRemoved :: (Ord a, Num a) => [a] -> Bool
isSafeWithOneRemoved l = go l (length l)
  where
    go _ 0 = False
    go ls n =
      isMonotonic (take (n - 1) ls ++ drop n ls)
        && diffBelow4 (take (n - 1) ls ++ drop n ls)
        || go ls (n - 1)

day2bsol :: IO ()
day2bsol = do
  reports <- readInp "input/day02.txt"

  let unsaveReps = unsafeReports reports
  let sol =
        numSafeReps reports
          + ( length
                . filter
                  isSafeWithOneRemoved
            )
            unsaveReps
  print sol