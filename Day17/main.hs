module Main where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQ
import Data.Functor
import Debug.Trace
type Pos = (Int, Int)
type Node = (Pos,Direction,Int)
type Graph = Map.Map Pos Int
type Distance = Map.Map Node Int
type Path = Map.Map Node Node
type Queue = PQ.MinPQueue Int Node
type Seen = Set.Set Node
-- each nodes has ~4 'from' direction, and they would be treated seperately, i.e, ((1,1), U) and ((1,1),D) are diffrent nodes

data Direction = U | D | L | R | Start deriving (Show, Eq, Ord)
isReturn :: Direction -> Direction -> Bool
isReturn U D = True
isReturn R L = True
isReturn D U = True
isReturn L R = True
isReturn _ _ = False

-- isValid :: [Direction] -> Bool
-- isValid [] = True
-- isValid [_] = True
-- isValid (a:b:c:d:_)= (not $ a == b && b == c && c == d ) && (not $ isReturn a b)

isValid :: Pos -> Int -> Int -> Bool
isValid (x,y) m n = if x >= 0 && x < n && y >= 0 && y < m then True else False


turn :: Direction -> [Direction]
turn U = [L,U,R]
turn D = [L,D,R]
turn L = [U,D,L]
turn R = [U,D,R]
turn Start = [D,R]

delta :: Direction -> (Int,Int)
delta U = (-1,0)
delta D = (1,0)
delta L = (0,-1)
delta R = (0,1)

move :: Node -> Direction -> Int -> Int  -> [Node]
move (p@(x,y),d,s) d' m n = if d == d' then
                              if s == 3 then []
                              else
                                let (dx,dy) = delta d'
                                    x' = x + dx
                                    y' = y + dy
                                in
                                  if isValid (x',y') m n then [((x',y'),d',(s+1))] else []
                            else
                              let (dx,dy) = delta d'
                                  x' = x + dx
                                  y' = y + dy
                                in
                                  if isValid (x',y') m n then traceShow (show dx)  $ [((x',y'),d',1)] else []


findNeibours :: Node -> Int -> Int -> [Node]
findNeibours curr@(p,d,s) m n = traceShowId $ concatMap (\i -> move curr i m n) $ turn d


                                  

-- findNeibour' grid (x, y) m n = let potential = concatMap (\i -> [((x-i,y),replicate i U),((x+i,y),replicate i D),((x,y-i),replicate i L),((x,y+i),replicate i R)]) [1..3]
--                                    positions = filter ((\(x,y) -> x >=0 && x < n && y >=0 && y < m ).fst) potential
--                                    vals = map (go grid) $ positions
--                                 in
--                                  traceShowId $ vals 
--   where
--     go grid (p,dirs)         = let d = head dirs
--                                    l = length dirs
--                                    c = case Map.lookup p grid of
--                                          Nothing -> error "Should not happen"
--                                          Just c -> c
--                                    v = calcLine grid d l p 0 c
--                                in
--                                  (p,dirs,v)
--     calcLine :: Graph -> Direction -> Int -> Pos -> Int -> Int -> Int
--     calcLine grid d l (x,y) index result = if index == (l-1) then result
--                                            else
--                                              let 
--                                                  adj = case d of
--                                                        L -> (x,y+1)
--                                                        R -> (x,y-1)
--                                                        U -> (x+1,y)
--                                                        D -> (x-1,y)
--                                                  loss = case Map.lookup adj grid of
--                                                           Nothing -> error $ "should not happen" ++ show adj
--                                                           Just c -> c
--                                              in
--                                                calcLine grid d l adj (index+1) (loss + result)


readInt :: String -> Int
readInt = read

main :: IO ()
main = readFile "sample.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn
--grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents
--solvePart1 ::
--solvePart1 :: [String] -> Path
solvePart1 content = let grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c) -> ((i,j),readInt $ singleton c)) $ zip [0..] s) $ zip [0..] content
                         n = length content
                         m = length $ head content
                         path = (solve grid m n)
                     in
                       --pathToList  (solve grid m n)  (0,0) ((n-1),(m-1))
                       --calc grid $ tail $ pathToList (solve grid m n)  (0,0) ((n-1),(m-1))
                       --path
                       path
                       --(n,m)


-- calc grid path = sum $ map (go grid) path
--   where go grid p = case Map.lookup p grid of
--                       Nothing -> error "should not happen"
--                       Just v -> v

--solve :: Graph -> Int -> Int -> Int
solve grid m n = let distance = Map.insert ((0,0),Start,0) 0 $ Map.fromList $ concatMap (\p -> [((p,U,1),maxBound),((p,D,1),maxBound),((p,L,1),maxBound),((p,R,1),maxBound)] ) $ Map.keys grid
                     queue =  PQ.insert 0 ((0,0),Start,0) $ PQ.empty
                     path = (Map.empty)
                     seen = Set.empty
                     --PQ.empty
                       --PQ.insert ((0,0),D) (0,[]) $ PQ.insert ((0,0),R) (0,[]) $ PQ.empty
                 in
                   go grid m n queue distance path seen
                   
  where
    --go:: Int -> Int -> Queue -> Distance -> Path -> Path
    go grid m n queue distance path seen = if PQ.null queue then distance
                                      else
                                        let (current@(_,currentNode),queue') = PQ.deleteFindMin queue
                                            ns = findNeibours currentNode m n
                                            (_,_,newQueue,newDistance,newPath,newSeen) = foldl' relaxing (current,grid,queue',distance,path,seen) ns
                                        in
                                          go grid m n newQueue newDistance newPath newSeen


--relaxing ::(Node, Graph, Queue, Distance,Path) -> Node -> ((Int,(Pos,Direction)), Graph, Queue, Distance,Path)
relaxing (current,grid,queue,distance,path,seen) next = let (currentVal,currentNode) = current
                                                            (pos,_,_) = next 
                                                            currentDist = case Map.lookup currentNode distance of
                                                                            Nothing -> error $ "could not find the" ++ show currentNode ++"in [distance], should not happen"
                                                                            Just dist  -> dist
                                                            targetVal = case Map.lookup next distance of
                                                                          Nothing -> maxBound
                                                                          Just val  -> val
                                                            weight =  case Map.lookup pos grid of
                                                                                  Nothing -> error "could not find the val of grid, should not happen"
                                                                                  Just w -> w


                                                               in
                                                                 if Set.member next seen then (current,grid,queue,distance,path,seen)  else
                                                        
                                                                   if currentDist + weight < targetVal then
                                                                     let newVal = currentDist + weight
                                                                         queue' = PQ.insert newVal next queue
                                                                         distance' = Map.insert next newVal distance
                                                                         path' = Map.insert next currentNode path
                                                                         seen' = Set.insert next seen
                                                                     in
                                                                       (current,grid,queue',distance',path',seen')
                                                                   else
                                                                     (current,grid,queue,distance,path,seen)

       
                                                          

