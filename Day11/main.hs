module Main where

import Data.List
import Data.Functor
main_part1 = readFile "sample.txt" <&> lines <&> expandUniverse <&> createGraph <&> solvePart1 <&> show >>= putStrLn


main = readFile "input.txt" <&> lines <&> solvePart2
expandUniverse = expandLine . transpose . expandLine . transpose 

expandLine :: [String] -> [String]
expandLine = concatMap (\x -> if all (== '.') x then [x,x] else [x])

-- naive
--expandLine' :: [String] -> [String]
--expandLine' = concatMap (\x -> if all (== '.') x then take 1000000 $ repeat x else [x])



type Pos = (Int,Int)
createGraph :: [String] -> [(Pos,Char)]
createGraph content = concatMap (\(j,s) -> map (\(i,c) -> ((i,j),c))$ zip [0..] s) $ zip [0..] content

--solvePart1 :: [(Pos,Char)] -> Int
solvePart1 graph = let galaxies = map fst $ filter ((=='#').snd) graph
                   in
                     go galaxies 0
           where
             go [] result = result
             go (x:xs) result = go xs (sum $ map (path x) xs )+ result


path :: Pos -> Pos -> Int
path (a,b) (a',b') = delta a a' + delta b b'
  where delta x y = if x < y then y - x else x -y


  

findEmptyRow :: [String] -> [Int]
findEmptyRow content = map fst $ filter (\(i,x) -> all (== '.') x) $ zip [0..] content


findEmptyCol = findEmptyRow .transpose 


solvePart2 contents = let n = 1000000
                          originalGraph = createGraph contents
                          galaxies = filter ((=='#').snd) originalGraph
                          emptyRows = findEmptyRow contents
                          emptyCols = findEmptyCol contents
                          galaxiesPos = map fst galaxies
                          newGalaxies' = foldr (expandRow n) galaxiesPos emptyRows
                          newGalaxies'' = foldr (expandCol n) newGalaxies' emptyCols
                      in
                          go newGalaxies'' 0
     where                          
             go [] result = result
             go (x:xs) result = go xs (sum $ map (path x) xs )+ result
expandRow ::Int -> Int -> [Pos] -> [Pos]
expandRow n x pos = map (\(i,j) -> if j > x then (i, j+ (n-1)) else (i,j)) pos

expandCol ::Int -> Int -> [Pos] -> [Pos]
expandCol n x pos = map (\(i,j) -> if i > x then (i+(n-1), j) else (i,j)) pos
