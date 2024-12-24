module Day23 (day23a, day23b) where

import Data.List (foldl', intersect, isPrefixOf, nub, maximumBy, sort, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Data.Graph (graphFromEdges)
import Data.Ord (comparing)

day23a :: IO ()
day23a = do
  contents <- readFile "input/Day23.txt"

  print $
    length$
    filter anyStartsWithT $
    map S.toList $
    nub $
      filter (\x -> S.size x >= 3) $
        concat $
          map snd $
            M.toList $
              findAllComponents $
                makeConnMap $
                  map
                    (splitOn "-")
                    $lines
                    contents

startsWithT :: [Char] -> Bool
startsWithT = isPrefixOf "t"

anyStartsWithT :: [[Char]] -> Bool
anyStartsWithT = any startsWithT

makeConnMap ls = foldl' insertOrExtend M.empty ls

-- | insert value only if not present in Map
insertOrExtend m l = case M.lookup (head l) m of
  Nothing -> case M.lookup (last l) m of
    Nothing ->
      M.insert (last l) (S.fromList (init l)) $
        M.insert (head l) (S.fromList (tail l)) m
    Just w -> M.insert (last l) (S.insert (head l) w) $ M.insert (head l) (S.fromList (tail l)) m
  Just v -> case M.lookup (last l) m of
    Nothing ->
      M.insert (last l) (S.fromList (init l)) $
        M.insert (head l) (S.insert (last l) v) m
    Just w ->
      M.insert (last l) (S.insert (head l) w) $
        M.insert (head l) (S.insert (last l) v) m

findComponents m x = case M.lookup x m of
  Nothing -> S.empty
  Just ls -> case M.lookup (head (S.toList ls)) m of
    Nothing -> S.empty
    Just l -> S.intersection l ls

findComponentsWVal m k ls = go m k (S.toList ls)
    where go m k [] = []
          go m k (x:xs) = case M.lookup x m of
            Nothing -> go m k xs 

            Just l ->
               ( map S.fromList $
                    zipWith (:) (intersect (S.toList  l) xs) (repeat [k, x])) ++( go m k xs)


findAllComponents m = M.mapWithKey (findComponentsWVal m) m

--part 2 

makeGraph adjacencyMap = G.graphFromEdges' $ map nodes  adjecencyList
    where adjecencyList = M.toList adjacencyMap
          nodes (x,y) = (x,x,S.toList y)
makeSCC adjacencyMap = G.stronglyConnCompR $ map nodes  adjecencyList
    where adjecencyList = M.toList adjacencyMap
          nodes (x,y) = (x,x,S.toList y)
day23b = do 
    contents <- readFile "input/Day23.txt"
    
    let connections = makeConnMap $
                  map
                    (splitOn "-")$lines contents
    let findAllInterconnectedAllKey = M.mapWithKey (findAllInterconnected connections) connections
    let interConnectedLs = map S.toList $map snd$ M.toList findAllInterconnectedAllKey
    let maxConnect = maximumBy (comparing length) interConnectedLs
    print $ intercalate ","$sort  maxConnect

findAllInterconnected m k ls  
    | S.null ls = S.singleton k
    |otherwise = case M.lookup (head(S.toList ls)) m of
            Nothing -> S.empty

            Just l -> S.insert k $
             
             findAllInterconnected  m (head (S.toList ls)) $
                S.intersection ls l 