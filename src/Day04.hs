module Day04
  ( findXmas,
    countXmasReverseTranspose,
    countXmasReverse,
    countXmasDiagUpLines,
    countXmasDiagRevLines,
    countXmasDiagRevUpLines,
    countAll,
    rotate1,
    example4,
    diag,
    day4asol,
    day4bsol,
  )
where

import Data.List (isInfixOf, isPrefixOf, tails, transpose)
import Data.List.HT (rotate)

example4 :: [Char]
example4 = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

--Part1
day4asol :: IO ()
day4asol = do
  input <- readFile "input/day04.txt"
  print $ countAll input

findXmas :: [Char] -> Bool
findXmas s = "XMAS" `isInfixOf` s

countXmas :: [Char] -> Int
countXmas s = length $ filter (isPrefixOf "XMAS") $ tails s

countXmasLines :: String -> Int
countXmasLines s = sum $ map countXmas $ lines s

countXmasTranspose :: String -> Int
countXmasTranspose s = sum $ map countXmas $ transpose $ lines s

countXmasReverse :: String -> Int
countXmasReverse s = sum $ map (countXmas . reverse) $ lines s

countXmasReverseTranspose :: String -> Int
countXmasReverseTranspose s = sum $ map (countXmas . reverse) $ transpose $ lines s

countXmasDiag :: [[Char]] -> Int
countXmasDiag s =
  sum $
    map countXmas $
      transpose $
        map (\x -> take (length x - 3) x) $
          take 4 $
            zipWith rotate [0 ..] s

countXmasDiagLines :: String -> Int
countXmasDiagLines s = sum $ map countXmasDiag $ tails $ lines s

diag = take 4 . map (zipWith rotate [0 ..]) $tails $ reverse $ lines example4

countXmasDiagUp :: [[Char]] -> Int
countXmasDiagUp s =
  sum $
    map countXmas $
      transpose $
        map (\x -> take (length x - 3) x) $
          take 4 $
            zipWith rotate [0 ..] s

countXmasDiagUpLines :: String -> Int
countXmasDiagUpLines s =
  sum $
    map countXmasDiagUp $
      tails $
        map reverse $
          reverse $
            lines s

countXmasDiagRev :: [[Char]] -> Int
countXmasDiagRev s =
  sum $
    map countXmas $
      transpose $
        map (\x -> take (length x - 3) x) $
          take 4 $
            zipWith
              rotate
              [0 ..]
              s

countXmasDiagRevLines :: String -> Int
countXmasDiagRevLines s =
  sum $
    map countXmasDiagRev $
      tails $
        map reverse $
          lines s

countXmasDiagRevUp :: [[Char]] -> Int
countXmasDiagRevUp s =
  sum $
    map countXmas $
      transpose $
        map (\x -> take (length x - 3) x) $
          take 4 $
            zipWith
              rotate
              [0 ..]
              s

countXmasDiagRevUpLines :: String -> Int
countXmasDiagRevUpLines s =
  sum $
    map countXmasDiagRevUp $
      tails $
        reverse $
          lines s

countAll :: String -> Int
countAll s =
  sum $
    map
      (\f -> f s)
      [ countXmasLines,
        countXmasTranspose,
        countXmasReverse,
        countXmasReverseTranspose,
        countXmasDiagLines,
        countXmasDiagUpLines,
        countXmasDiagRevLines,
        countXmasDiagRevUpLines
      ]

rotate1 :: [a] -> [a]
rotate1 = rotate 1

-- Part 2

day4bsol :: IO ()
day4bsol = do
  input <- readFile "input/day04.txt"
  let inpWCoords = addCoordinates $ lines input
  print $
    length $
      filter (isMas inpWCoords) $
        map fst $
          filter (not . isBorder . fst) $
            filter ((== 'A') . snd) inpWCoords

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [String] -> [((Int, Int), Char)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

isMas l (x, y) =
  ( ( 'M' == (snd $ head $ filter ((== (x -1, y -1)) . fst) l)
        && 'S' == (snd $ head $ filter ((== (x + 1, y + 1)) . fst) l)
    )
      || ( 'S' == (snd $ head $ filter ((== (x -1, y -1)) . fst) l)
             && 'M' == (snd $ head $ filter ((== (x + 1, y + 1)) . fst) l)
         )
  )
    && ( ( 'M' == (snd $ head $ filter ((== (x -1, y + 1)) . fst) l)
             && 'S' == (snd $ head $ filter ((== (x + 1, y -1)) . fst) l)
         )
           || ( 'S' == (snd $ head $ filter ((== (x -1, y + 1)) . fst) l)
                  && 'M' == (snd $ head $ filter ((== (x + 1, y -1)) . fst) l)
              )
       )

isBorder (x, y) = x `elem` [1, 140] || y `elem` [1, 140]