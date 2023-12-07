module Main where

import Data.Char
import Data.Functor
wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


solvePart1 :: [String] -> Int
solvePart1 contents = let (a:b:_) = contents
                          times = getList a
                          distances = getList b
                          sets = zip times distances
                      in
                        product $ map solve sets
                            where getList str = map read $ words $ trim $ last $ wordsWhile (== ':') str
                                  solve :: (Int,Int) -> Int
                                  solve (a,b) = length $ [i | i <- [1..a], i * (a-i) > b] 


solvePart2 contents = let (a:b:_) = contents
                          times = getList a
                          distances = getList b
                          sets = zip times distances
                      in
                        product $ map solve sets
                            where getList str = [read $ filter (/= ' ') $ last $ wordsWhile (== ':') str]
                                  solve :: (Int,Int) -> Int
                                  solve (a,b) = length $ [i | i <- [1..a], i * (a-i) > b] 


main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn 
