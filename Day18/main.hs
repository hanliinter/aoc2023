module Main where

import Data.Functor
import Data.List
import qualified Data.Vector as Vector
import Data.Vector((!))
main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn

type Pos = (Int,Int)
data Direction = U | R | L | D deriving (Show, Eq, Ord)
--instance Read Direction where
readDir "R" = R
readDir "U" = U
readDir "L" = L
readDir "D" = D

readD '0' = R
readD '1' = D
readD '2' = L
readD '3' = U
--readDir
  
type Color = String
type Instruction =  (Direction,Int)

process str = let r = readDir $ head $ words str
                  i = read $ head $ tail $ words str
                  c = head $ tail $ tail $ words str
               in
                (r,i)

process' :: String -> Instruction
process' str = let c = head $ tail $ tail $ words str
                   (d:v) =  tail $ reverse $ tail c
                   ('#':h) = reverse v
               in
                 (readD d, read $ "0x" ++ h)

                
--solvePart1                 
solvePart1 contents = let (e,edge,b) = solve $ map process contents
                          a = abs $ shoelace $ Vector.fromList $ reverse edge
                      in
                        ((a - b + 2) `div` 2 ) + b

solvePart2 contents = let (e,edge,b) = solve $ map process' contents
                          a = abs $ shoelace $ Vector.fromList $ reverse edge
                      in
                        ((a - b + 2) `div` 2 ) + b                        
-- because Area A = 1/2 \Sum * yi (xi+1 - xi-1), use 2A here to avoid float number
shoelace :: Vector.Vector (Int,Int) -> Int
shoelace v = let n = Vector.length v -1
               in go v n 0 0
  where go v n i result = if i == n then result
                          else
                            let (x1,y1) = v ! i
                                (x2,y2) = v ! (i+1)
                                result' = result + (x1 * y2 - x2 * y1)
                             in
                              go v n (i+1) result'
                            
-- isInside :: Pos -> [Pos] -> Bool
-- isInside (x,y) edge = let possible elements = filter (\(i,j) -> j == y && i <= x) edge
--                       in
--                         if (x,y) `elem` elements then True
--                         else
--                           groupBy (\a b -> fst a == fst)


-- --findGrid :: [Pos] -> (Pos,Pos,Pos,Pos)
-- findGrid p = let x1 = minimum (map fst p)
--                  x2 = maximum (map fst p)
--                  y1 = minimum (map snd p)
--                  y2 = maximum (map snd p)
--               in
-- --               ((x1,y1),(x1,y2),(x2,y1),(x2,y2))
--                   (x1,x2,y1,y2)

step :: Direction -> Int -> (Int,Int)
step U i = (-i,0)
step D i = (i,0)
step L i = (0,-i)
step R i = (0,i)

--solve = undefined
solve :: [Instruction] -> ((Int,Int),[(Int,Int)],Int)
solve = foldl' go ((0,0),[(0,0)],0)
  where go ((x,y),result,b) (direction,i) =    let (dx,dy) = step direction i
                                                   (x',y') = (x + dx, y+dy)
                                                   in
                                                 ((x',y'), (x',y'):result,b+i)
                                                 

           
