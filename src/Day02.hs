module Day02
  ( day2asol,
    readInp,
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
    isMonotonicIncreasing [x] = True
    isMonotonicIncreasing l = and $ zipWith (<) l (map head $ tail $ init $ tails l)
    isMonotonicDecreasing [x] = True
    isMonotonicDecreasing l = and $ zipWith (>) l (map head $ tail $ init $ tails l)

diffBelow4 :: (Ord a, Num a) => [a] -> Bool
diffBelow4 l = and $ map ((<= 3) . abs) $zipWith (-) l (map head $ tail $ init $ tails l)

-- | filter reports that ar monotonic
day2asol :: IO ()
day2asol = do
  reports <- readInp "input/day02.txt"
  print (length reports)
  print (filter (\x -> isMonotonic x && diffBelow4 x) reports)
  let sol = (length . filter (\x -> isMonotonic x && diffBelow4 x)) reports
  print sol