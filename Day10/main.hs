module Main where

import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Debug.Trace

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn


--solvePart1 :: [String] -> Int
solvePart1 content = let  dict = filter ((/= Ground).snd) $ markPos content
                          source@((i,j), _) = head $ filter ((== Source).snd) $ dict
                          routes = getRoute' source dict
                      in
                          maximum $ map (\x -> (length x +1) `div` 2) $ map (solve dict source) routes
   where
     solve dict node p = go dict node p []
     go dict node p@(_,v) result = if v == Source
                                   then node:result
                                   else go dict p (findNext dict node p) (node:result)

solvePart2 content = let  dict = filter ((/= Ground).snd) $ markPos content
                          source@((i,j), _) = head $ filter ((== Source).snd) $ dict
                          routes = getRoute' source dict
                          mainloop =  head $ map (solve dict source)  routes
                          fields = [v | v <- (markPos content), v `notElem` mainloop]
                          --n = length content
                          --m = length $ head content
                      in
                       
                        length $ filter (\(p,_) -> isInside p mainloop) fields
    
                        -- map (\(p,_) -> isInside p ( mainloop)) fields
                       --fields                       -- reverse mainloop 
                       --map length  mainloop
                       --
   where
     solve dict node p = go dict node p []
     go dict node p@(_,v) result = if v == Source
                                   then (node:result)
                                   else go dict p (findNext dict node p) (node:result)

-- printLoop loop m n = go loop 0 0 m n []
--            where
--              go :: [(Pos,Value)]-> Int -> Int -> Int -> Int -> [String] -> [String]
--              go loop i j m n result = if j == n
--                                           then reverse result
--                                           else if i == m then
--                                                      let
--                                                        (r:rest) = result
--                                                        result' = (reverse $ ('\n':r)):rest
--                                                      in
--                                                        go loop 0 (j+1) m n result'
--                                                else
--                                                  let c =case lookup (i,j) loop of
--                                                         Nothing -> '.'
--                                                         Just _  -> '*'
--                                                  in
--                                                    if result == []
--                                                    then go loop (i+1) j m n  [[c]]
--                                                    else let (r:rest) = result
--                                                             result' = (c:r):rest
--                                                         in
--                                                           go loop (i+1) j m n result'
                                                   
                                                   

isInside (i,j) loop = let ws =  length $ filter (\((x,y), v)-> (x< i && y ==j) && v `elem` [NS,SE,SW]) loop
                       in   ws `mod` 2 == 1
                        --map (\s -> ((i,j),s)) [ws,es]
                        --((i,j), ws)

type Pos = (Int,Int)
data Value = NS   -- '|'
              | NE -- 'F'
              | NW  -- '7'
              | SE  -- 'L'
              | SW  -- 'J'
              | WE  -- '-'
              | Ground
              | Source -- 'S'
              deriving (Show, Eq)

charToValue :: Char -> Value
charToValue '|' = NS
charToValue 'F' = NE
charToValue '7' = NW
charToValue 'L' = SE
charToValue 'J' = SW
charToValue '-' = WE
charToValue 'S' = Source
charToValue '.' = Ground

step :: (Pos,Value)-> (Pos,Value) ->Pos
step ((i,j),_)  ((i',j'),v) =  case v of
                             NS -> head $ (i,j) `delete` [(i',j'+1), (i',j'-1)] 
                             NE -> head $ (i,j) `delete` [(i'+1,j'), (i',j'+1)] 
                             NW -> head $ (i,j) `delete` [(i'-1,j'), (i',j'+1)] 
                             SE -> head $ (i,j) `delete` [(i',j'-1), (i'+1,j')] 
                             SW -> head $ (i,j) `delete` [(i',j'-1), (i'-1,j')] 
                             WE -> head $ (i,j) `delete` [(i'+1,j'), (i'-1,j')] 


findNext dict current next = case lookup (step current next) dict of
                               Nothing -> error ("go to the dead end" ++ show current)
                               Just v -> ((step current next),v)
                             
getRoute ((i,j),Source) dict =  mapMaybe (\x -> case lookup x dict of
                                                  Nothing -> Nothing
                                                  Just v -> Just (x,v)) [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]


getRoute' ((i,j),Source) dict =let
                                 left = case lookup (i-1,j) dict of
                                          Nothing -> Nothing
                                          Just v -> if v == NE || v == SE || v == WE then Just ((i-1,j),v) else Nothing
                                 right = case lookup (i+1,j) dict of
                                          Nothing -> Nothing
                                          Just v -> if v == NW || v == SW || v == WE then Just ((i+1,j),v) else Nothing
                                 up = case lookup (i,j-1) dict of
                                          Nothing -> Nothing
                                          Just v -> if v == NS || v == NE || v == NW then Just ((i,j-1),v) else Nothing
                                 down = case lookup (i,j+1) dict of
                                          Nothing -> Nothing
                                          Just v -> if v == NS || v == SW || v == SE then Just ((i,j+1),v) else Nothing
                              in
                                 catMaybes [left,right,up,down]

--markPos :: [String] -> [(Pos,Value)]
markPos content = concatMap (\(j,str) -> map (\(i,c) -> ((i,j),charToValue c))$ zip [0..] str )  $ zip [0..] content

squeezeH xs = sum $ expand $ go xs []
      where go [] result = result
            go (x:xs) [] = go xs [(x,1)]
            go (x:xs) ((r,n):rs) = if (fst x == fst r +1) then
                                 go xs ((x,n+1):rs)
                               else go xs ((x,1):(r,n):rs)
            expand xs = map (\(_,c) -> if c > 1 then 2 else 1 ) xs

squeezeV xs = go xs []
      where go [] result = result
            go (x:xs) [] = go xs [x]
            go (x:xs) (r:rs) = if (snd x == snd r +1) then
                                 go xs (x:rs)
                               else go xs (x:r:rs)
