module Day14 (coord100ex, day14a, day14aex, day14b, treeAnimateFromFile) where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import Graphics.Gloss
import Graphics.Gloss.Data.Color

type Coord = (Int, Int)

type RobotInstructions = (Coord, Coord)

coord100ex (x, y) (xv, yv) = ((x + (xv * 100)) `mod` 11, (y + (yv * 100)) `mod` 7)

coord100 (x, y) (xv, yv) = ((x + (xv * 100)) `mod` 101, (y + (yv * 100)) `mod` 103)

coord1 (x, y) (xv, yv) = ((x + xv) `mod` 101, (y + yv) `mod` 103)

coordNStep (x, y) (xv, yv) n = ((x + (xv * n)) `mod` 101, (y + (yv * n)) `mod` 103)

allRobotsNStep robots n = map (flip (uncurry coordNStep) n) robots

parseInputNums :: String -> [[Int]]
parseInputNums = map (map read . splitOn (",") . drop 1 . dropWhile (/= '=')) . words

extractPositionX :: String -> Int
extractPositionX = head . head . parseInputNums

extractPositionY :: String -> Int
extractPositionY = last . head . parseInputNums

extractVelX :: String -> Int
extractVelX = head . last . parseInputNums

extractVelY :: String -> Int
extractVelY = last . last . parseInputNums

parseRobot :: String -> RobotInstructions
parseRobot s = ((extractPositionX s, extractPositionY s), (extractVelX s, extractVelY s))

parseAllRobots :: String -> [RobotInstructions]
parseAllRobots = map parseRobot . lines

robotPos100 = map (uncurry coord100) . parseAllRobots

robotPos100ex = map (uncurry coord100ex) . parseAllRobots

inQuadrant1 = length . filter (\(x, y) -> x < 50 && y < 51)

inQuadrant2 = length . filter (\(x, y) -> x > 50 && y < 51)

inQuadrant3 = length . filter (\(x, y) -> x < 50 && y > 51)

inQuadrant4 = length . filter (\(x, y) -> x > 50 && y > 51)

inQuadrant1ex = length . filter (\(x, y) -> x < 5 && y < 3)

inQuadrant2ex = length . filter (\(x, y) -> x > 5 && y < 3)

inQuadrant3ex = length . filter (\(x, y) -> x < 5 && y > 3)

inQuadrant4ex = length . filter (\(x, y) -> x > 5 && y > 3)

toChar ls x = if x `elem` ls then 'l' else '.'

makeLineToPlot ls n =
  map (toChar ls) (zip (repeat n) [0 .. 100])
    ++ "\n"

makeLinesToPlot ::
  (Foldable t, Eq a, Eq b, Num b, Num a, Enum b, Enum a) =>
  t (a, b) ->
  [Char]
makeLinesToPlot ls = foldl' (\acc x -> acc ++ makeLineToPlot ls x) "" [10 .. 80]

day14a :: IO ()
day14a = do
  contents <- readFile "input/day14.txt"
  let positions = robotPos100 contents
  let res =
        product
          [ inQuadrant1 positions,
            inQuadrant2 positions,
            inQuadrant3 positions,
            inQuadrant4 positions
          ]
  print res

day14aex :: IO ()
day14aex = do
  contents <- readFile "input/day14ex.txt"
  let positions = robotPos100ex contents
  let res =
        product
          [ inQuadrant1ex positions,
            inQuadrant2ex positions,
            inQuadrant3ex positions,
            inQuadrant4ex positions
          ]
  print res

day14b :: IO ()
day14b = do
  contents <- readFile "input/day14.txt"
  let robots = parseAllRobots contents
  mapM_ (plotNthConfig robots) [8000 .. 8010]

plotNthConfig :: (Show b, Integral b) => [((b, b), (b, b))] -> b -> IO ()
plotNthConfig robots n =
  putStrLn $
    (show n)
      ++ (makeLinesToPlot $ allRobotsNStep robots n)

-- Animation
backgroundColor :: Color
backgroundColor = makeColor 255 0 0 255

treeColor :: Color
treeColor = makeColor 0 255 0 255

treeAnimate :: [RobotInstructions] -> IO ()
treeAnimate robots =
  animate (InWindow "Tree" (1350, 1350) (105, 105)) backgroundColor $
    drawSquares robots

treeAnimateFromFile :: String -> IO ()
treeAnimateFromFile filename = do
  contents <- readFile filename
  let robots = parseAllRobots contents
  treeAnimate robots

time2step :: Float -> Int
time2step = floor

coord2Square :: Coord -> Picture
coord2Square (x, y) =
  translate (fromIntegral (-5 * x)) (fromIntegral (-5 * y)) $
    (color treeColor) (rectangleSolid 5 5)

drawSquares :: [RobotInstructions] -> Float -> Picture
drawSquares robots time =
  pictures $
    text (show (time2step (5 * time)))
      : ( map coord2Square $
            allRobotsNStep robots $
              (time2step (5 * time))
        )
