module Day24 (testParseLineOp, day24ex, day24b, toBinary, findInday24bMap) where

import Data.Foldable (Foldable (foldl'))
import Data.List (isPrefixOf, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M

data Wire
  = LIT Bool
  | AND Wire Wire
  | OR Wire Wire
  | XOR Wire Wire

data WireMap
  = Lit Bool
  | And String String
  | Or String String
  | Xor String String
  deriving (Show, Read, Eq)

eval :: Wire -> Bool
eval (LIT b) = b
eval (AND b1 b2) = eval b1 && eval b2
eval (OR b1 b2) = eval b1 || eval b2
eval (XOR b1 b2) = eval b1 /= eval b2

evalWireMap :: M.Map String WireMap -> String -> Bool
evalWireMap m s = case M.lookup s m of
  Nothing -> False
  Just (Lit b) -> b
  Just (And s1 s2) -> evalWireMap (M.insert s (Lit (s1eval && s2eval)) mNew) s
    where
      s1eval = evalWireMap m s1
      s2eval = evalWireMap m1 s2
      m1 = M.insert s1 (Lit s1eval) m
      mNew = M.insert s2 (Lit s2eval) m1
  Just (Or s1 s2) -> evalWireMap (M.insert s (Lit (s1eval || s2eval)) mNew) s
    where
      s1eval = evalWireMap m s1
      s2eval = evalWireMap m1 s2
      m1 = M.insert s1 (Lit s1eval) m
      mNew = M.insert s2 (Lit s2eval) m1
  Just (Xor s1 s2) ->
    evalWireMap (M.insert s (Lit (s1eval /= s2eval)) mNew) s
    where
      s1eval = evalWireMap m s1
      s2eval = evalWireMap m1 s2
      m1 = M.insert s1 (Lit s1eval) m
      mNew = M.insert s2 (Lit s2eval) m1

parseLineLit :: [Char] -> M.Map [Char] WireMap -> M.Map [Char] WireMap
parseLineLit l m = M.insert (head ls) (Lit (toBool (last ls))) m
  where
    ls = splitOn (": ") l

parseLineOp :: [Char] -> M.Map [Char] WireMap -> M.Map [Char] WireMap
parseLineOp l m = M.insert (last ls) ((findOp ls1) (head ls1) (last ls1)) m
  where
    ls = splitOn (" -> ") l
    ls1 = words $ head ls

    findOp ls2 = toOp (ls2 !! 1)

toOp :: [Char] -> String -> String -> WireMap
toOp "AND" = And
toOp "OR" = Or
toOp "XOR" = Xor

toBool :: [Char] -> Bool
toBool "1" = True
toBool "0" = False

testParseLineOp :: M.Map [Char] WireMap
testParseLineOp = parseLineOp "x00 AND y00 -> z00" M.empty

evalAllZs :: M.Map String WireMap -> [Bool]
evalAllZs m = map (evalWireMap m) zkeys
  where
    zkeys =
      sort $
        filter (isPrefixOf "z") $
          M.keys m

makeWireMap ::
  (Foldable t1, Foldable t2) =>
  t1 [Char] ->
  t2 [Char] ->
  M.Map [Char] WireMap
makeWireMap lits ops =
  foldl'
    (flip parseLineLit)
    (foldl' (flip parseLineOp) M.empty ops)
    lits

bool2Num :: Num p => Bool -> p
bool2Num False = 0
bool2Num True = 1

toDecimal :: (Integral t, Num p) => t -> [p] -> p
toDecimal _ [] = 0
toDecimal n (x : xs) =
  x * (2 ^ n)
    + toDecimal (n + 1) xs
toBinary :: Integral a => a -> [a]
toBinary 0 = []
toBinary n = n `mod` 2 : toBinary (n `div` 2)

day24ex :: IO ()
day24ex = do
  contents <- readFile "input/day24.txt"
  let splitOnBlank = splitOn "\n\n" contents
  let literals = lines $ head splitOnBlank
  let ops = lines $ last splitOnBlank
  let m = makeWireMap literals ops
  print $
    toDecimal 0 $
      map bool2Num $
        evalAllZs m

-- PArt 2 

evalAll :: [Char] -> M.Map String WireMap -> [Bool]
evalAll c m = map (evalWireMap m) zkeys
  where
    zkeys =
      sort $
        filter (isPrefixOf c) $
          M.keys m

evalAllAsBinary :: Num b => [Char] -> M.Map String WireMap -> [b]
evalAllAsBinary c m = map bool2Num $ evalAll c m 

evalAllAsDecimal c m = toDecimal 0 $ evalAllAsBinary c m 

differences m = abs (allZs - (allYs + allXs))
    where allZs = evalAllAsDecimal "z" m 
          allYs = evalAllAsDecimal "y" m 
          allXs = evalAllAsDecimal "x" m 

day24b :: IO ()
day24b = do
  contents <- readFile "input/day24.txt"
  let splitOnBlank = splitOn "\n\n" contents
  let literals = lines $ head splitOnBlank
  let ops = lines $ last splitOnBlank
  let m = makeWireMap literals ops
  print m 
  print $M.lookup "z11" m 
  print $M.lookup "z31" m 
  print $M.lookup "z38" m 
  print $M.lookup "z45" m 
  print $ toBinary $ differences m 

--11, 31, 38, 45

findInday24bMap :: [Char] -> IO ()
findInday24bMap s = do
  contents <- readFile "input/day24.txt"
  let splitOnBlank = splitOn "\n\n" contents
  let literals = lines $ head splitOnBlank
  let ops = lines $ last splitOnBlank
  let m = makeWireMap literals ops
  print $ M.lookup s m 
  
--z31, dmh, z38,dvq, rpv, z11, rpb, ctg
-- ctg,dmh,dvq,rpb,rpv,z11,z31,z38

--z31 And x31 y31
--ctw: XOR x31 y31
--bvk: And x38 y38 
--z38 bvk or trm 
--bvk to hhv
--"dmh",Xor "fgs" "ctw"
--"bmm",And "fgs" "ctw"
--"dtf",Or "dmh" "bmm"
--"z11",And "dvh" "hnn"
--"rpv",Xor "hnn" "dvh"
--"dvq",Xor "hhv" "pqr"
--"ctg",And "y15" "x15"
--"rpb",Xor "x15" "y15"
