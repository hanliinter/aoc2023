module Main where

import Data.Functor
import Data.List (nub,sort,foldl')
import Debug.Trace
import qualified Data.Set as Set
import Data.Vector((!))
import qualified Data.Vector as Vector
import Control.Monad.State

newtype Queue a = Queue ([a],[a])

push :: Eq a => Queue a -> a -> Queue a
push (Queue (a,b)) n = Queue (a, (n:b))

pushList :: Eq a => Queue a -> [a] -> Queue a
pushList (Queue (a,b)) b' = Queue (a, reverse b' ++ b)

pop :: Eq a => Queue a-> (a,Queue a)
pop (Queue (a,b)) = if a == [] then (head $ reverse b,Queue ((tail $ reverse b),[] ))
                       else (head a, Queue (tail a, b))

instance Functor Queue where
  fmap f (Queue (a,b)) = Queue (map f a, map f b) 

empty::Eq a => Queue a
empty = Queue ([],[])

isEmpty :: Eq a => Queue a -> Bool
isEmpty (Queue (a,b)) = a == [] && b == []

singleton :: Eq a => a -> Queue a
singleton a = Queue ([a],[])


main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn
type Pos = (Int,Int)
type Grid =  [(Pos,Char)]
markPos :: [String] -> [(Pos,Char)]
markPos contents = concatMap (\(i,xs) -> map (\(j,c) -> ((i,j),c))  $ zip [0..] xs)  $ zip [0..] contents

expand ::Grid -> Int -> Int -> Pos -> [Pos]
expand grid m n (x,y) = filter (go grid m n) [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
  where go grid m n (x,y) = case lookup (x ,y) grid of
                              Nothing -> traceShow (x,y) False
                              Just c -> c /= '#'

expand' ::Grid -> Pos -> [Pos]
expand' grid (x,y) = filter (go grid) [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
  where go grid (x,y) = case lookup (x ,y) grid of
                              Nothing ->  False
                              Just c -> c /= '#'


type Seen = Vector.Vector Bool
type Graph a = State Seen a
type VGrid = Vector.Vector Char

expandState :: VGrid -> Int -> [Pos] -> Graph [Pos]
expandState vgrid n poss = foldM (go n vgrid) [] poss
  where go :: Int -> VGrid -> [Pos] -> Pos -> Graph [Pos]
        go n vgrid current (x,y) = do
          seen <- get
          let candidates = [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
          let result = filter (\(x,y) -> x >=0 && y >= 0 && x < n && y < n && (seen ! (x*n +y)) == False && (vgrid ! (x*n +y)) /= '#') candidates
          let seen'  = foldl'  go' seen result
          put seen'
          return $ current ++ result
        go' seen (x,y) = Vector.update seen (Vector.singleton (x*n +y,True))


solveState :: VGrid -> Int -> Int -> Pos -> Graph Int
solveState vgrid size step start =  go [] vgrid [start] size step step
   where go pos_even vgrid queue size step i = if i == 0 then return $ length pos_even
                                               else do
                                                    newQueue <- expandState vgrid size queue
                                                    if i `mod` 2 /= 0 then
                                                      let pos_even' = newQueue ++ pos_even in
                                                        go pos_even' vgrid newQueue size step (i-1)
                                                      else
                                                      go pos_even vgrid  newQueue size step (i-1)
                                               
                                              


--expandVector :: Vector.Vector Char -> Pos -> 


expandSet :: Grid -> Int -> Int -> Pos -> Set.Set Pos
expandSet grid m n (x,y) = Set.filter (go grid m n) $ Set.fromList [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
  where go grid m n (x,y) = case lookup (x `mod` n,y `mod` m) grid of
                              Nothing -> False
                              Just c -> c /= '#'


expandSetWithinGrid :: Grid -> Pos -> Set.Set Pos
expandSetWithinGrid grid (x,y) = Set.filter (go grid) $ Set.fromList [((x+1),y),((x-1) ,y),(x,(y+1)),(x,(y-1) )]
  where go grid (x,y) = case lookup (x ,y) grid of
                              Nothing -> False
                              Just c -> c /= '#'



expandSetSquare :: Grid ->  Pos -> Set.Set Pos
expandSetSquare grid p = expandSetWithinGrid grid p


--solvePart1 :: [String] -> Int
solvePart1 contents = let grid = markPos contents
                          n = length contents
                          m = length $ head contents
                          start = fst $ head $ filter ((=='S').snd) grid
                      in
                        length $ go [start] grid n m 64 0
  where go :: [Pos] -> Grid -> Int -> Int -> Int -> Int -> [Pos]
        go positions grid m n s i = if i == s then positions
                                    else let positions' = nub $ sort $ concatMap (expand grid m n) positions in
                                           go positions' grid m n s (i+1)


solve :: Grid -> Int -> Pos -> Int -> Int
solve grid size start step = go []  [start] grid size step 0
  where go :: [Pos] -> [Pos] -> Grid -> Int -> Int -> Int -> Int
        go pos_even queue grid size step i = if i == step then length pos_even
                                             else
                                              let newQueue = (traceShow $ length queue) $ nub $ sort $ concatMap (expand' grid) queue in
                                               if i `mod` 2 /= 0 then
                                                 let pos_even' = nub $ sort $ pos_even ++ newQueue  in
                                                     go pos_even' newQueue grid size step (i+1)
                                               else
                                                     go pos_even newQueue grid size step (i+1)


markPosVector :: [String] -> Vector.Vector Char
markPosVector contents = Vector.concat $ map (Vector.fromList) contents


solvePart2 contents = let vgrid = markPosVector contents
                          grid = markPos contents
                          n = length contents
                          start = fst $ head $ filter ((=='S').snd) grid
                          
                          seen = Vector.map (\_ -> False) vgrid
                      in
                        --solve grid n start 132
                        --solveState :: VGrid -> Int -> Int -> Pos -> Graph Int
                        evalState (solveState vgrid n 64 start) seen
