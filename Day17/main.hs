module Main where

import Data.List
import qualified Data.Map as Map
import Data.Functor

type Pos = (Int, Int)
type Graph = Map.Map Pos Int
type Distance = Map.Map Pos (Int,[Direction])
type Path = Map.Map Pos Pos
type Queue = Map.Map Int Pos 

data Direction = U | D | L | R deriving (Show, Eq)
isReturn :: Direction -> Direction -> Bool
isReturn U D = True
isReturn R L = True
isReturn D U = True
isReturn L R = True
isReturn _ _ = False


isValid :: [Direction] -> Bool
isValid [] = True
isValid [_] = True
isValid (a:b:c:d:_)= (not $ a == b && b == c && c == d ) && (not $ isReturn a b) 
isValid (x:y:_) = not $ isReturn x y

--isValid _ = True

pathToList :: Path -> Pos -> Pos -> [Pos]
pathToList  p s t = go p s t [t]
  where go p s t result = case Map.lookup t p of
                            Nothing -> error "Should not happen"
                            Just t' -> if t' == s then (s:result)
                                                  else
                                         go p s t' (t':result)


findNeibour :: Pos -> Int -> Int -> [(Pos,Direction)]
findNeibour (x, y) m n = filter ((\(x,y) -> x >=0 && x < n && y >=0 && y < m ).fst)  [((x-1,y),U),((x+1,y),D),((x,y-1),L),((x,y+1),R)]

main :: IO ()
main = readFile "small.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn
--grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents
--solvePart1 ::
--solvePart1 :: [String] -> Path
solvePart1 content = let grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c) -> ((i,j),read $ singleton c)) $ zip [0..] s) $ zip [0..] content
                         n = length content
                         m = length $ head content
                         path = (solve grid m n)
                     in
                       --pathToList  (solve grid m n)  (0,0) ((n-1),(m-1))
                       --calc grid $ tail $ pathToList (solve grid m n)  (0,0) ((n-1),(m-1))
                       path
                       --(n,m)


calc grid path = sum $ map (go grid) path
  where go grid p = case Map.lookup p grid of
                      Nothing -> error "should not happen"
                      Just v -> v

--solve :: Graph -> Int -> Int -> Int
solve grid m n = let distance = Map.insert (0,0) (0,[]) $ Map.fromList $ map (\p -> (p,(maxBound,[]))) $ Map.keys grid in
                   go m n (Map.singleton 0 (0,0)) distance (Map.empty)
                   
  where
    --go:: Int -> Int -> Queue -> Distance -> Path -> Path
    go m n queue distance path = if Map.null queue then distance
                                 else
                                   let (next@(_,p),queue') = Map.deleteFindMin queue
                                       ns = findNeibour p m n
                                       (_,_,newQueue,newDistance,newPath) = foldl' relaxing (next,grid,queue',distance,path) ns
                                    in
                                     go m n newQueue newDistance newPath


relaxing ::((Int,Pos), Graph, Queue, Distance,Path) -> (Pos,Direction) -> ((Int,Pos), Graph, Queue, Distance,Path)
relaxing (current,grid,queue,distance,path) (pos,d) = let (currentVal,currentPos) = current
                                                          (currentDist,currentDirections) = case Map.lookup currentPos distance of
                                                                 Nothing -> error "could not find the pos in distance, should not happen"
                                                                 Just (dist,directions)  -> (dist,directions)
                                                          (targetVal,targetDirections) = case Map.lookup pos distance of
                                                                 Nothing -> error "could not find the pos in distance, should not happen"
                                                                 Just (val,directions)  -> (val,directions)
                                                          weight = case Map.lookup pos grid of
                                                                 Nothing -> error "could not find the val of grid, should not happen"
                                                                 Just w -> w

                                                                 
                                                       in
                                                        if currentVal > currentDist then error "nonono" else
                                                        
                                                          if currentDist + weight <= targetVal && isValid (d:currentDirections) then
                                                            let newVal = currentDist + weight
                                                                queue' = Map.insert newVal pos queue
                                                                distance' = Map.insert pos (newVal,(d:currentDirections)) distance
                                                                path' = Map.insert pos currentPos path
                                                            in
                                                              (current,grid,queue',distance',path')
                                                          else
                                                            (current,grid,queue,distance,path)

       
                                                          

