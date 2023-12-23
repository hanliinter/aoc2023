module Main where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQ
import Data.Functor
import Debug.Trace
type Pos = (Int, Int)
type Graph = Map.Map Pos Int
type Distance = Map.Map (Pos,Direction) (Int,[Direction])
type Path = Map.Map (Pos,Direction) (Pos,Direction)
type Queue = PQ.MinPQueue Int (Pos,Direction)
type Seen = Set.Set (Pos,Direction)
-- each nodes has ~4 'from' direction, and they would be treated seperately, i.e, ((1,1), U) and ((1,1),D) are diffrent nodes

data Direction = U | D | L | R deriving (Show, Eq, Ord)
isReturn :: Direction -> Direction -> Bool
isReturn U D = True
isReturn R L = True
isReturn D U = True
isReturn L R = True
isReturn _ _ = False


isValid :: [Direction] -> Bool
--isValid xs = if (length $ take 3 xs)  < 3 
isValid [] = True
isValid [_] = True
isValid (a:b:c:d:_)= (not $ a == b && b == c && c == d ) && (not $ isReturn a b)
--isValid (a:b:c:d:_)= (not $ a == b && b == c && c == d ) && (not $ isReturn a b) 
isValid (x:y:_) = not $ isReturn x y




--isValid _ = True

pathToList :: Path -> Pos -> (Pos,Direction) -> [(Pos,Direction)]
pathToList  p s t = go p s t [t]
  where go p s t result = case Map.lookup t p of
                            Nothing -> error "Should not happen"
                            Just t' -> if (fst t') == s then (t':result)
                                       else
                                         go p s t' (t':result)


findNeibour :: Pos -> Int -> Int -> [(Pos,Direction)]
findNeibour (x, y) m n = filter ((\(x,y) -> x >=0 && x < n && y >=0 && y < m ).fst)  [((x-1,y),U),((x+1,y),D),((x,y-1),L),((x,y+1),R)]


findNeibour' :: Graph -> Pos -> Int -> Int -> [(Pos,[Direction],Int)]
findNeibour' grid (x, y) m n = let potential = concatMap (\i -> [((x-i,y),replicate i U),((x+i,y),replicate i D),((x,y-i),replicate i L),((x,y+i),replicate i R)]) [1..3]
                                   positions = filter ((\(x,y) -> x >=0 && x < n && y >=0 && y < m ).fst) potential
                                   vals = map (go grid) $ positions
                                in
                                 traceShowId $ vals 
  where
    go grid (p,dirs)         = let d = head dirs
                                   l = length dirs
                                   c = case Map.lookup p grid of
                                         Nothing -> error "Should not happen"
                                         Just c -> c
                                   v = calcLine grid d l p 0 c
                               in
                                 (p,dirs,v)
    calcLine :: Graph -> Direction -> Int -> Pos -> Int -> Int -> Int
    calcLine grid d l (x,y) index result = if index == (l-1) then result
                                           else
                                             let 
                                                 adj = case d of
                                                       L -> (x,y+1)
                                                       R -> (x,y-1)
                                                       U -> (x+1,y)
                                                       D -> (x-1,y)
                                                 loss = case Map.lookup adj grid of
                                                          Nothing -> error $ "should not happen" ++ show adj
                                                          Just c -> c
                                             in
                                               calcLine grid d l adj (index+1) (loss + result)

--findNeibour' :: Grid -> Pos -> Direction -> (Int,Int) -> Int -> Int -> [(Pos,Direction)]
--findNeibour' grid (x,y) dirt range m n = 

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


calc grid path = sum $ map (go grid) path
  where go grid p = case Map.lookup p grid of
                      Nothing -> error "should not happen"
                      Just v -> v

--solve :: Graph -> Int -> Int -> Int
solve grid m n = let distance = Map.insert ((0,0),D) (0,[]) $ Map.insert ((0,0),R) (0,[]) $ Map.fromList $ concatMap (\p -> [((p,U),(maxBound,[])),((p,D),(maxBound,[])),((p,L),(maxBound,[])),((p,R),(maxBound,[]))] ) $ Map.keys grid
                     queue = PQ.insert 0 ((0,0),D)  $ PQ.insert 0 ((0,0),R) $ PQ.empty
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
                                        let (current@(_,(p,d)),queue') = PQ.deleteFindMin queue
                                            ns = findNeibour' grid p m n
                                            (_,_,newQueue,newDistance,newPath,newSeen) = foldl' relaxing (current,grid,queue',distance,path,seen) ns
                                        in
                                          go grid m n newQueue newDistance newPath newSeen


--relaxing ::((Int,(Pos,Direction)), Graph, Queue, Distance,Path) -> (Pos,Direction) -> ((Int,(Pos,Direction)), Graph, Queue, Distance,Path)
relaxing (current,grid,queue,distance,path,seen) (pos,dirs,val) = let (currentVal,currentNode) = current
                                                                      d = head dirs
                                                                      (currentDist,currentDirections) = case Map.lookup currentNode distance of
                                                                        Nothing -> error "could not find the pos in distance, should not happen"
                                                                        Just (dist,directions)  -> (dist,directions)
                                                                      (targetVal,targetDirections) = case Map.lookup (pos,d) distance of
                                                                        Nothing -> error "could not find the pos in distance, should not happen"
                                                                        Just (val,directions)  -> (val,directions)
                                                                      weight = val -- case Map.lookup pos grid of
                                                                 -- Nothing -> error "could not find the val of grid, should not happen"
                                                                 -- Just w -> w


                                                               in
                                                                 if Set.member (pos,d) seen then (current,grid,queue,distance,path,seen)  else
                                                        
                                                                   if currentDist + weight < targetVal  && isValid (dirs ++ currentDirections) then
                                                                     let newVal = currentDist + weight
                                                                         queue' = PQ.insert newVal (pos,d) queue
                                                                         distance' = Map.insert (pos,d) (newVal,(dirs ++ currentDirections)) distance
                                                                         path' = Map.insert (pos,d) currentNode path
                                                                         seen' = Set.insert (pos,d) seen
                                                                     in
                                                                       (current,grid,queue',distance',path',seen')
                                                                   else
                                                                     (current,grid,queue,distance,path,seen)

       
                                                          

