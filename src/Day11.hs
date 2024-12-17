module Day11 (blink, day11a, day11b) where

import Data.IntMap.Merge.Lazy (contramapFirstWhenMatched)
import Data.List (foldl', nub)
import qualified Data.Map as M

blinkOnce :: (Foldable t, Eq b, Num b, Show b, Read b) => t b -> [b]
blinkOnce l = concatMap updateOne l

updateOne :: (Eq a, Num a, Show a, Read a) => a -> [a]
updateOne 0 = [1]
updateOne n
  | (even . length . show) n =
    [ (read . (\x -> take (length x `div` 2) x) . show) n,
      (read . (\x -> drop (length x `div` 2) x) . show) n
    ]
  | otherwise = [2024 * n]

blink :: (Eq b, Num b, Show b, Read b) => Int -> [b] -> [b]
blink n l = iterate blinkOnce l !! n

blink1 n s = blink n [s]

day11a :: IO ()
day11a =
  readFile "input/day11.txt"
    >>= (print . length . blink 25 . map read . words)

dictupdate (s, dict) = (news, newdict)
  where
    newdict = foldl' insertIfAbsent dict s
    news = concatMap (flip (M.findWithDefault []) newdict) s

dictupdateIter n l = iterate dictupdate l !! n

dictupdate25 (s, dict) = (news, newdict)
  where
    newdict = foldl' insertIfAbsent25 dict s
    news = concatMap (flip (M.findWithDefault []) newdict) s

dictupdateIter25 n l = iterate dictupdate25 l !! n

count el = length . filter (== el)

day11b = do
  contents <- readFile "input/day11.txt"
  let nums = map read $ words contents
  let blink25 = dictupdateIter 5 (nums, M.empty)
  let blink75 = dictupdateIter25 3 blink25
  print $length $ fst blink75

insertIfAbsent :: (Ord b, Num b, Show b, Read b) => M.Map b [b] -> b -> M.Map b [b]
insertIfAbsent m s = case M.lookup s m of
  Nothing -> M.insert s (blink1 5 s) m
  Just _ -> m

insertIfAbsent25 :: (Ord b, Num b, Show b, Read b) => M.Map b [b] -> b -> M.Map b [b]
insertIfAbsent25 m s = case M.lookup s m of
  Nothing -> M.insert s (blink1 25 s) m
  Just _ -> m

memoizedStone ntimes stonemap stone = findInStoneMap (stone, ntimes) stonemap
  where
    findInStoneMap (s, n) m = case M.lookup (s, n) m of
      Nothing -> concatMap (memoizedStone (n -1) (M.insert (s, 1) (updateOne s) m)) (updateOne s)
      Just ls -> ls