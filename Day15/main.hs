module Main where
import Data.Char
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Debug.Trace


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


main :: IO ()
main = readFile "input.txt" <&> solvePart2  <&> show >>= putStrLn

--solvePart1 :: String -> Int
solvePart1 s = sum $ map hash $ wordsWhile (==',') $ head $ lines s
hash :: String -> Int
hash = foldl go 0
  where go  current c = (17 * (current + (ord c))) `mod` 256

--solvePart2 :: String -> [(Int,[(String,Int)])]
solvePart2 s = let instructions = wordsWhile (==',') $ head $ lines s
                   initialMap = Map.fromList $ map (\x -> (x,[])) [0..255]
                in
                sum $ map calcPower $ filter ((/=[]).snd) $ Map.toList $ foldl solve initialMap instructions

calcPower :: (Int,[(String,Int)]) -> Int
calcPower (i,xs) = sum $ map (* (i+1))  $ zipWith (\j (_,f) -> (j * f))  [1..] xs
                
solve :: Map.Map Int [(String,Int)] -> String -> Map.Map Int [(String,Int)]
solve m instr =     if last instr == '-'
                    then let 
                             label = reverse $ tail $ reverse instr
                             boxId = hash label
                          in
                           Map.update (\v -> Just $ (deleteBy (\a b -> fst a == fst b) (label,0) v)) boxId m
                      else
                      
                         let label = head $ wordsWhile (=='=') instr
                             boxId = hash label
                             focusLength = read $ last $ wordsWhile (=='=') instr
                          in
                            Map.update (\v -> Just $ (insertOrUpdate v (label,focusLength))) boxId m

                      
insertOrUpdate :: Eq a=> [(a,b)] -> (a,b) -> [(a,b)]
insertOrUpdate oldList newItem = go oldList newItem []
  where
    go [] newItem result = reverse (newItem:result)
    go (o:os) newItem result =  if fst newItem == fst o
                                then
                                  reverse (newItem:result) ++ os
                                else
                                  go os newItem (o:result)
