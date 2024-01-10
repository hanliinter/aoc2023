{-# LANGUAGE BlockArguments #-}
module Main where
import Debug.Trace
import Data.Functor
import Data.List
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad.State
main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn

type Pos = (Int,Int)
type Grid = [(Pos,Char)]

--markPos :: [String] -> [(Pos,Char)]
markPos contents = concatMap (\(i,s) -> map (\(j,c) -> ((i,j),c)) $ zip [0..] s)$ zip [0..] contents

getNeibours :: Pos -> Grid -> Int -> Int -> [Pos]
getNeibours (x,y) grid n m = let ps = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                                 result = filter (go grid n m) ps
                              in
                               result
  where go grid n m (x,y) = if x < 0 || x >=n || y < 0 || y >= m || (fromJust $ lookup (x,y) grid) == '#' then False else True

getValidNeibours :: Pos -> Grid -> Int -> Int -> [Pos]
getValidNeibours (x,y) grid n m = let ps = case fromJust $ lookup (x,y) grid of
                                             '>' -> [(x,y+1)]
                                             'v' -> [(x+1,y)]
                                             '<' -> [(x,y-1)]
                                             '^' -> [(x-1,y)]
                                             '.' -> [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                                      result = filter (go grid n m) ps
                                  in
                                    result
  where go grid n m (x,y) = if x < 0 || x >=n || y < 0 || y >= m || (fromJust $ lookup (x,y) grid) == '#' then False else True                             

getKeyPoints :: Grid -> Int -> Int -> [Pos]
getKeyPoints pts n m = map fst $ filter (go pts n m) pts
  where go pts n m (pos,c) = if c == '#' then False
                             else let neibours = getNeibours pos pts n m in
                                    if length neibours >= 3 then True else False



getExits :: Grid -> Int  -> [Pos]
getExits grid n = map fst $ filter (\((x,y),c) -> (x == 0 && c == '.') || (x == (n-1) && c == '.'))  grid


type Edges = Map Pos [(Pos,Int)]
findEdges :: Pos ->[Pos] -> Grid -> Int -> Int -> Edges
findEdges pts points grid n m = go points pts [(pts,0)] grid n m Set.empty Map.empty
                            
  where go :: [Pos] ->Pos -> [(Pos,Int)] -> Grid -> Int ->Int -> Set.Set Pos -> Edges -> Edges
        go points pt queue grid n m seen result = if null queue then result
                                                  else
                                                    let (h@(p,c):rest) = queue
                                                        neibours = getValidNeibours p grid n m
                                                        nextPos =  filter (\x -> not (x `Set.member` seen)) neibours
                                                        nextNode =  map (\p -> (p,c+1)) $ nextPos
                                                        newSeen =  Set.insert p $ Set.union seen $ Set.fromList nextPos
                                                        nextNode' = (filter (\(p,c) -> p `elem` points) nextNode)
                                                        newQueue = if null nextNode' then nextNode ++ rest else rest
                                                        newResult = if null nextNode' then result else Map.insertWith (++) pt nextNode' result
                                                    in
                                                      go points pt newQueue grid n m newSeen newResult


findEdges' :: Pos ->[Pos] -> Grid -> Int -> Int -> Edges
findEdges' pts points grid n m = go points pts [(pts,0)] grid n m Set.empty Map.empty
                            
  where go :: [Pos] ->Pos -> [(Pos,Int)] -> Grid -> Int ->Int -> Set.Set Pos -> Edges -> Edges
        go points pt queue grid n m seen result = if null queue then result
                                                  else
                                                    let (h@(p,c):rest) = queue
                                                        neibours = getNeibours p grid n m
                                                        nextPos =  filter (\x -> not (x `Set.member` seen)) neibours
                                                        nextNode =  map (\p -> (p,c+1)) $ nextPos
                                                        newSeen =  Set.insert p $ Set.union seen $ Set.fromList nextPos
                                                        nextNode' = (filter (\(p,c) -> p `elem` points) nextNode)
                                                        newQueue = if null nextNode' then nextNode ++ rest else rest
                                                        newResult = if null nextNode' then result else Map.insertWith (++) pt nextNode' result
                                                    in
                                                      go points pt newQueue grid n m newSeen newResult




dfs :: Edges -> [Pos] -> Pos -> Pos -> Int
dfs edge pts start end = go edge pts [] start end 0 
  where go edges pts path current end result = if current == end then result
                                               else
                                                 let targets = fromJust $ Map.lookup current edges
                                                     results = map (\(p,n) -> go edges pts (current:path) p end (result + n)) targets
                                                 in
                                                   maximum results


type Seen = Set Pos
dfsState :: Edges -> [Pos] -> Pos -> Pos -> State ([Pos], Int,Seen) ()
dfsState edge pts start end = go edge pts start end 0 
  where go edges pts current end result = do
                                               (pos,currMax,seen) <- get
                                               let seen' = Set.insert current seen
                                               if current `Set.member` seen then return () else
                                                 if current == end then do
                                                  
                                                  if result > currMax then put (current:pos,result,seen)  else return ()
                                               else
                                                do
                                                 let targets = fromJust $ Map.lookup current edges
                                                 put ((current:pos),currMax,seen') 
                                                 mapM_  (\(p,n) -> go edges pts p end (result + n)) targets
                                                 (pos',currMax',_) <- get
                                                 put (pos',currMax',seen)


                          
                             


solvePart1 contents = let grid = markPos contents
                          n = length contents
                          m = length $ head contents
                          inout = getExits grid n
                          pts = inout ++ getKeyPoints grid n m
                          edge =  Map.unions $ map (\x -> findEdges x pts grid n m ) pts

                       in
                        dfs edge pts (head inout) (head $ tail inout)
                       
                        
solvePart2 contents = let grid = markPos contents
                          n = length contents
                          m = length $ head contents
                          inout = getExits grid n
                          pts = inout ++ getKeyPoints grid n m
                          edge =  Map.unions $ map (\x -> findEdges' x pts grid n m ) pts
                          (_,i,_) = execState (dfsState edge pts (head inout) (head $ tail inout)) ([],0,Set.empty)

                       in
                        i
                        --edge
                        
-- use brute force
-- use toplogical sort then DFS




