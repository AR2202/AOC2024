module Day07 (day7a, day7b, solveDay7) where

data ResultTree = Leaf Int | Node Int ResultTree ResultTree deriving (Show, Read, Eq)

day7a :: IO ()
day7a = solveDay7 "input/day07.txt" >>= print
day7b :: IO ()
day7b = solveDay7b "input/day07.txt" >>= print
parseAsResult :: [Char] -> Integer
parseAsResult = read . takeWhile (/= ':')

parseAsOperands :: [Char] -> [Integer]
parseAsOperands = map read . words . drop 1 . dropWhile (/= ':')

-- | checks if condition is unsatisfyable - 
-- | this is not 100% correct if there are 1s in the list
outsideBounds :: (Ord a, Foldable t, Num a) => a -> t a -> Bool
outsideBounds res list = (sum list > res || product list < res) && 1 `notElem` list

solveDay7 :: FilePath -> IO Integer
solveDay7 fname = do
  contents <- readFile fname
  let results = map parseAsResult $lines contents
  let operands = map parseAsOperands $ lines contents
  -- filtering only the ones that are within bounds
  let withinBounds = map not $ zipWith outsideBounds results operands
  let resultsInBounds = map snd $ filter  fst $ zip withinBounds results
  let operandsInBounds = map snd $ filter fst $ zip withinBounds operands
  let satisfied = zipWith (satisfiable 0) operandsInBounds resultsInBounds
  let correctRes = map snd $ filter fst $ zip satisfied resultsInBounds
  return $ sum correctRes
  

solveDay7b :: FilePath -> IO Integer
solveDay7b fname = do
  contents <- readFile fname
  let results = map parseAsResult $lines contents
  let operands = map parseAsOperands $ lines contents 
  let satisfied = zipWith (satisfiable 0) operands results
  let correctRes = map snd $ filter fst $ zip satisfied results
  let incorrectRes = map snd $ filter fst $ zip ( map not satisfied) results
  let incorrectOp = map snd $ filter fst $ zip (map not satisfied) operands
  let additionalSatisfiable = zipWith (satisfiableWConcat 0) incorrectOp incorrectRes
  let additionalSatisfiableRes = map snd $ filter fst $ zip additionalSatisfiable incorrectRes
  return $sum additionalSatisfiableRes + sum correctRes
-- | check if condition is satisfiable by testing all possible combinations
satisfiable :: (Eq a, Num a) => a -> [a] -> a -> Bool
satisfiable curr [] t = curr == t
satisfiable curr (x : xs) t =
  satisfiable (x + curr) xs t
    || satisfiable (x * curr) xs t

-- Part 2 

satisfiableWConcat curr [] t = curr == t
satisfiableWConcat curr (x : xs) t =
  satisfiableWConcat (x + curr) xs t
    || satisfiableWConcat (x * curr) xs t
    || satisfiableWConcat (read (show curr ++ show x)) xs t 