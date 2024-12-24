module Day22
  ( day22a,
    combineMaps,
    changeMap,
    changeMap',
    changes,
    sequences,
    calculateSecret,
    calculateSecret2000,
    solve22b,
    day22b
  )
where

import Data.Bits
import Data.List (foldl', tails)
import qualified Data.Map as M

day22a :: IO ()
day22a = do
  contents <- readFile "input/Day22.txt"
  let nums = map read $ lines contents
  let secretNum2000 = map calculateSecret2000 nums
  print $ sum secretNum2000

secret1 n = (64 * n) `xor` n

prune n = n `mod` 16777216

secret2 n = (n `div` 32) `xor` n

secret3 n = (n * 2048) `xor` n

calculateSecret :: Integer -> Integer
calculateSecret = prune . secret3 . prune . secret2 . prune . secret1

calculateSecret2000 :: Integer -> Integer
calculateSecret2000 n = (iterate calculateSecret n) !! 2000

-- part 2
lastDigit :: Integral a => a -> a
lastDigit n = n `mod` 10

sequences :: Integer -> [Integer]
sequences = map lastDigit . iterate calculateSecret

changes :: Num a => [a] -> [[a]]
changes l = map (take 4) . tails $ zipWith (-) (tail l) l

changeMap l = M.fromList $ zip (changes l) (drop 4 l)

--changeMap' :: (Foldable t, Ord k) => t (k, a) -> M.Map k a
changeMap' :: (Ord a, Num a) => [a] -> M.Map [a] a
changeMap' l = foldl' insertMaybe M.empty $zip (changes l) (drop 4 l)

-- | insert value only if not present in Map
insertMaybe :: Ord k => M.Map k a -> (k, a) -> M.Map k a
insertMaybe m (k, v) = case M.lookup k m of
  Nothing -> M.insert k v m
  Just _ -> m

makeChangeMap :: Integer -> M.Map [Integer] Integer
makeChangeMap n = changeMap' $ take 2000 $ sequences n

combineMaps :: (Foldable f, Ord k, Num a) => f (M.Map k a) -> M.Map k a
combineMaps = M.unionsWith (+)
findMaxVal :: (Ord a, Num a) => M.Map k a -> a
findMaxVal  = M.foldl' max 0 

solve22b :: [Integer] -> Integer
solve22b l = findMaxVal $combineMaps $ map makeChangeMap l 

day22b :: IO ()
day22b = do 
    contents <- readFile "input/Day22.txt"
    print $ solve22b $ map read $ lines contents