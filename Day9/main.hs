module Main where

import Data.Functor
import Debug.Trace

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn

solvePart1 :: [String] -> Int
solvePart1 content = sum $ map solvePart1OneLine content

solvePart1OneLine :: String -> Int
solvePart1OneLine l = go (map read $ words l) 0
  where  go [] _ = error "Could not find the next value"
         go xs acc = if all (==0) xs then acc
                                    else go (zipWith (-) (tail xs) xs)  acc + last xs



solvePart2 content =  sum $ map solvePart2OneLine content


solvePart2OneLine :: String -> Int
solvePart2OneLine l = let nums = map read $ words l
                      in last $ scanl (flip (-)) 0 $ go nums [head nums]
  where  go [] _ = error "Could not find the next value"
         go xs acc = if all (==0) xs then acc
                                    else let ys = zipWith (-) (tail xs) xs
                                          in go (ys)  (head ys:acc)
