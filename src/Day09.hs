module Day09
  ( example,
    splitFileBlocks,
    fileBlocks,
    spaceBlocks,
    spaceslist,
    filelists,
    moveAllFiles,
    checksum,
    day9a,
    moveAllFilesOnce,
    exampleb,
    findFilePos,
    filePositions,
    spacePositions,
    checkSumP2,
    day9b,
  )
where

import Data.List
import Data.List.Split

example :: [Char]
example = "2333133121414131402"

exampleb :: [Char]
exampleb = "00992111777.44.333....5555.6666.....8888.."

splitFileBlocks :: String -> [(Int, [Int])]
splitFileBlocks = zip [0 ..] . chunksOf 2 . map (read . return)

fileBlocks :: (a, [Int]) -> [a]
fileBlocks (i, l) = replicate (head l) i

filelists :: String -> [[Int]]
filelists = map fileBlocks . splitFileBlocks

spaceBlocks :: (a1, [a2]) -> [a2]
spaceBlocks = tail . snd

spaceslist :: String -> [Int]
spaceslist = concatMap spaceBlocks . splitFileBlocks

revFileList :: String -> [Int]
revFileList = reverse . concat . filelists

moveAllFiles :: String -> [Int]
moveAllFiles s =
  take (length $ revFileList s) $
    moveFiles (filelists s) (spaceslist s) (revFileList s) (length $ revFileList s)

moveFiles :: [[a]] -> [Int] -> [a] -> Int -> [a]
moveFiles _ _ _ 0 = []
moveFiles f [] r n = take n (concat f)
moveFiles f (s : ss) r n =
  head f
    ++ take s r
    ++ moveFiles (tail f) ss (drop s r) (n - (length (head f) + s))

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0 ..]

day9a :: IO ()
day9a = do
  contents <- readFile "input/day09.txt"
  print $ checksum $ moveAllFiles contents

-- Part 2

updateSpaces :: Foldable t => [Int] -> t a -> [Int]
updateSpaces [] _ = []
updateSpaces (s : ss) r
  | s >= length r = (s - length r) : ss
  | otherwise = s : updateSpaces ss r

moveWholeFiles _ [] = []
moveWholeFiles [] rs = concat rs
moveWholeFiles (s : ss) (r : rs)
  -- if there wasn't any space, put r file at the end
  | s : ss == updateSpaces (s : ss) r = moveWholeFiles (s : ss) rs ++ r
  | otherwise = moveWholeFiles (s : ss) rs ++ r

moveAllFilesOnce s =  findFilePos (spacePositions s) ((reverse . filePositions) s) []

filePosSize (i, l) = (i, (head l))

spacePosSize (i, l) = (i, last l)

findFilePos _ [] _ = []
findFilePos [] ((fid, (j, r)) : rs) unsuccessful = (fid, (j, r)) : findFilePos (reverse unsuccessful) rs []
findFilePos ((i, s) : ss) ((fid, (j, r)) : rs) unsuccessful
  | i > j = (fid, (j, r)) : findFilePos (reverse unsuccessful ++ ((i, s) : ss)) rs []
  | s > r = (fid, (i, r)) : findFilePos (reverse unsuccessful ++ ((i + r, s - r) : ss)) rs []
  | s == r = (fid, (i, r)) : findFilePos (reverse unsuccessful ++ ss) rs []
  | otherwise = findFilePos ss ((fid, (j, r)) : rs) ((i, s) : unsuccessful)

findPos :: [Char] -> [Int]
findPos s = map sum $inits $ makeIntList s

makeIntList :: [Char] -> [Int]
makeIntList = map (read . return)

posSize :: [Char] -> [(Int, Int)]
posSize s = zip (findPos s) (makeIntList s)

filePositions :: [Char] -> [(Int, (Int, Int))]
filePositions = zip [0 ..] . map head . chunksOf 2 . posSize

spacePositions :: [Char] -> [(Int, Int)]
spacePositions = init . map last . chunksOf 2 . posSize

singleChecksum :: (Num a, Enum a) => (a, (a, Int)) -> a
singleChecksum (fid, (startpos, size)) = sum $ zipWith (*) [startpos ..] $replicate size fid

checkSumP2 :: [(Int, (Int, Int))] -> Int
checkSumP2 = sum . map singleChecksum

day9b :: IO ()
day9b = do
  contents <- readFile "input/day09.txt"
  print $ checkSumP2 $ moveAllFilesOnce contents