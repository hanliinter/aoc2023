module Main where

import Data.Functor
import Data.List
import Debug.Trace
import qualified Data.Set as Set

main :: IO ()
main = readFile "sample.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn
type Pos = (Int,Int)
type Grid =  [(Pos,Char)]
markPos :: [String] -> [(Pos,Char)]
markPos contents = concatMap (\(i,xs) -> map (\(j,c) -> ((i,j),c))  $ zip [0..] xs)  $ zip [0..] contents

expand ::Grid -> Int -> Int -> Pos -> [Pos]
expand grid m n (x,y) = filter (go grid m n) [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
  where go grid m n (x,y) = case lookup (x `mod` n,y `mod` m) grid of
                              Nothing -> traceShow (x,y) False
                              Just c -> c /= '#'



expandSet :: Grid -> Int -> Int -> Pos -> Set.Set Pos
expandSet grid m n (x,y) = Set.filter (go grid m n) $ Set.fromList [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
  where go grid m n (x,y) = case lookup (x `mod` n,y `mod` m) grid of
                              Nothing -> traceShow (x,y) False
                              Just c -> c /= '#'





--solvePart1 :: [String] -> Int
solvePart1 contents = let grid = markPos contents
                          n = length contents
                          m = length $ head contents
                          start = fst $ head $ filter ((=='S').snd) grid
                      in
                        length $ go [start] grid n m 10 0
  where go :: [Pos] -> Grid -> Int -> Int -> Int -> Int -> [Pos]
        go positions grid m n s i = if i == s then positions
                                    else let positions' = nub $ sort $ concatMap (expand grid m n) positions in
                                           go positions' grid m n s (i+1)



solvePart2 contents = let grid = markPos contents
                          n = length contents
                          m = length $ head contents
                          start = fst $ head $ filter ((=='S').snd) grid
                          (s_even,s_odd) = go  (Set.empty) (Set.empty) (Set.singleton start) grid n m 500 0
                      in
                          (Set.size s_even , Set.size s_odd)
  where go :: Set.Set Pos -> Set.Set Pos -> Set.Set Pos -> Grid -> Int -> Int -> Int -> Int -> (Set.Set Pos,Set.Set Pos)
        go pos_even pos_odd queue grid m n s i = if i == s then (pos_even,pos_odd)
                                           else if i `mod` 2 == 0 then
                                                  let pos_odd' = Set.union pos_even $ Set.unions $ Set.map (expandSet grid m n) queue
                                                      newQueue = Set.unions $ Set.map (expandSet grid m n) queue
                                                  in
                                                      go pos_even pos_odd' newQueue grid m n s (i+1)
                                                else
                                                  let pos_even' = Set.union pos_even $ Set.unions $ Set.map (expandSet grid m n) queue 
                                                      newQueue = Set.unions $ Set.map (expandSet grid m n) queue
                                                  in
                                                      go pos_even' pos_odd newQueue grid m n s (i+1)
