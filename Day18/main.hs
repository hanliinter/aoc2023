module Main where

main :: IO ()
main = undefined
data Direction = U | R | L | D deriving (Show, Eq, Ord)
type Color = String
type Instruction =  (Direction,Int,Color)

step :: Direction -> Int -> (Int,Int)
step 

--solve :: [Instruction] -> Int
solve = foldl' go ((0,0),[])
  where go ((x,y),result) (direction,i) = 
