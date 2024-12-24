module Day24 (testParseLineOp, day24ex) where

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
