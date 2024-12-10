module Day10 (day10a, day10b) where

import Data.List
import qualified Data.Map as M

readInt :: String -> Int
readInt = read

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [[a]] -> [((Int, Int), a)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

coordmap :: String -> M.Map (Int, Int) Int
coordmap s =
  M.fromList $
    addCoordinates $
      map (map (readInt . return)) $
        lines s

nonDiagNeighbors ::
  (Num a, Num b, Enum a, Enum b) =>
  (a, b) ->
  [(a, b)]
nonDiagNeighbors (x, y) =
  [(x, w) | w <- [y -1 .. y + 1]]
    ++ [(u, y) | u <- [x -1 .. x + 1]]

nextStep ::
  (Ord a, Ord b1, Num b2, Num a, Num b1, Enum a, Enum b1, Eq b2) =>
  M.Map (a, b1) b2 ->
  ((a, b1), b2) ->
  [((a, b1), b2)]
nextStep m (coord, height) =
  zip
    ( filter (\c -> M.lookup c m == Just (height + 1)) $
        nonDiagNeighbors coord
    )
    (repeat (height + 1))

trails ::
  ( Eq b2,
    Foldable t,
    Ord a1,
    Ord b1,
    Num b2,
    Num a1,
    Num b1,
    Enum a1,
    Enum b1
  ) =>
  ([((a1, b1), b2)] -> t a2) ->
  M.Map (a1, b1) b2 ->
  [((a1, b1), b2)] ->
  Int
trails f m [] = 0
trails f m ls
  | [] /= (filter (\x -> snd x == 9) ls) = length (f ls)
  | otherwise = trails f m (concatMap (nextStep m) ls)

trailsFromOne f m zero = trails f m [zero]

day10a :: IO ()
day10a = day10 nub "input/day10.txt"

day10b :: IO ()
day10b = day10 id "input/day10.txt"

day10 ::
  Foldable t =>
  ([((Int, Int), Int)] -> t a) ->
  FilePath ->
  IO ()
day10 f fname = do
  contents <- readFile fname
  print $
    sum $
      map
        (trailsFromOne f (coordmap contents))
        ( M.toList $
            M.filter (== 0) $
              coordmap contents
        )
