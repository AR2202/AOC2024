module Day15 (parseInput, solveDay15a, day15a) where

import Data.List (foldl', transpose)
import qualified Data.Map as M
import Text.Parsec.Pos (initialPos)
import Data.List.Split(splitOn)
import Text.Parsec (parseTest)
import Test.Hspec (after)

data Direction = Up | Down | LEFT | RIGHT deriving (Show, Read, Eq)

toDirection '<' = LEFT
toDirection '>' = RIGHT
toDirection 'v' = Down
toDirection '^' = Up

nextPos (x, y) Up = (x, y -1)
nextPos (x, y) Down = (x, y + 1)
nextPos (x, y) RIGHT = (x + 1, y)
nextPos (x, y) LEFT = (x -1, y)

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [[a]] -> [((Int, Int), a)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

makeMap :: String -> M.Map (Int, Int) Char
makeMap s = M.fromList $ addCoordinates $ lines s

oneMove pos m direction = case M.lookup nextpos m of
  Nothing -> (pos, m)
  Just '.' -> (nextpos, M.insert nextpos '@' (M.insert pos '.' m))
  Just '#' -> (pos, m)
  Just 'O' -> case M.lookup nextpos (push nextpos nextpos m direction) of
    Just '.' -> (nextpos, M.insert nextpos '@' (M.insert pos '.' (push nextpos nextpos m direction)))
    _ -> (pos, m)
  _ -> (pos,m)
  where
    nextpos = nextPos pos direction

push startpos pos m direction = case M.lookup nextpos m of
  Nothing -> m
  Just '.' -> M.insert nextpos 'O' (M.insert startpos '.' m)
  Just '#' -> m
  Just 'O' -> push startpos nextpos m direction
  where
    nextpos = nextPos pos direction

allMoves initialPos initialMap directions = foldl' (uncurry oneMove) (initialPos, initialMap) directions

gps (x,y) = 100 * (y -1)+ x -1 

allGPS m = map (gps . fst) $ filter ((==)'O' . snd) $ M.toList m

findRobot m = fst $ head $ filter ((==) '@' . snd) $M.toList m 
parseInput fname = do 
    contents <- readFile fname 
    let parts = splitOn "\n\n" contents
    let directions = concat $ lines $ last parts
    let map = M.fromList $ addCoordinates $ lines $ head parts 
    return (map, directions) 


solveDay15a :: FilePath -> IO ()
solveDay15a fname = do 
    (mapCoord,directionstr)<- parseInput fname 
    let directions = map toDirection directionstr
    let posRobot = findRobot mapCoord
    let (endpos,afterAllMoves) = allMoves posRobot mapCoord directions  
    let sumGPS = sum $ allGPS afterAllMoves 
    print sumGPS
 
day15a :: IO ()
day15a = solveDay15a "input/day15.txt"