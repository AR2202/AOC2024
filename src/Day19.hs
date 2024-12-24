module Day19(matchIndices, canMatch, isPossibleDesign, day19a, day19example) where

import Data.List(isPrefixOf)
import Data.List.Split(splitOn)

matchIndices str substr = go substr str (length str - length substr)
    where go sub s (-1) = []
          go sub s i = if sub `isPrefixOf` (drop i s) then (i, i+length sub ): go sub s (i - 1) else go sub s (i-1)
canMatch indexlist strlen = go indexlist strlen 0
    where go indexlist strlen startindex
            | strlen - 1 == startindex = True
            | strlen - 1 < startindex = False
            | filter ((==)startindex . fst) indexlist == [] = False 
            | otherwise = or $ map (go indexlist strlen . snd) $  filter ((==)startindex . fst) indexlist

numMatch indexlist strlen = go indexlist strlen 0
    where go indexlist strlen startindex
            | strlen  == startindex = 1
            | filter ((==)startindex . fst) indexlist == [] = 0 
            | otherwise = sum $ map  (go indexlist strlen . snd) $  filter ((==)startindex . fst) indexlist

isPossibleDesign patterns design = canMatch allIndices (length design)
    where allIndices = concatMap (matchIndices design) patterns
numPossibleDesigns patterns design = numMatch allIndices (length design)
    where allIndices = concatMap (matchIndices design) patterns

solveDay19 filepath = do
    contents <- readFile filepath 
    let patterns = splitOn (", ") $ head $ lines contents
    let designs = drop 2 $ lines contents
    let possiblePatterns = filter (isPossibleDesign patterns) designs
    let part1Sol = length possiblePatterns
    let part2Sol =  sum $map (numPossibleDesigns patterns) possiblePatterns
    print part1Sol
    print part2Sol

day19a = solveDay19 "input/day19.txt"
day19example = solveDay19 "input/day19ex.txt"