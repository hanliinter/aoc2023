module Main where

import Data.List
import Data.Char
import Debug.Trace

main :: IO ()
main = lines <$> readFile "input.txt" >>= solvePart2

solvePart1 :: [String] -> IO ()
solvePart1 contents = putStrLn $ show $ sum $ map go contents
  where go :: String -> Int
        go str = let cardId = head $ wordsWhile (== ':') str
                     gameSet = trim $ last $ wordsWhile (== ':') str
                     winSet = words $ head $ wordsWhile (== '|') gameSet
                     ownSet = words $ last $ wordsWhile (== '|') gameSet
                     n = length $ filter (`elem` winSet) ownSet
                 in
                   if n == 0 then 0 else 2 ^ (n-1)

solvePart2 = putStrLn . show .part2 .solve


solve :: [String] -> [Int]
solve contents =  map go contents
 where go :: String -> Int
       go str = let cardId = head $ wordsWhile (== ':') str
                    gameSet = trim $ last $ wordsWhile (== ':') str
                    winSet = words $ head $ wordsWhile (== '|') gameSet
                    ownSet = words $ last $ wordsWhile (== '|') gameSet
                    n = length $ filter (`elem` winSet) ownSet
                 in
                    n

part2 xs = let n = length xs
               xs' = zip [1..] xs
           in
                  go xs' xs' 0
  where go :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int
        go old [] result = result
        go old ((i,m):ys) result = if m == 0
                               then go old ys (result +1)
                               else go old ((take m $ drop i old)++ys) (result+1)

-- even you don't win the game, you still have one 'origin' instance
-- solvePart2 xs = let 
--                     xs' = zipWith (\a b -> (a,b,0)) [1..] xs
--                 in
--                   go (traceShowId xs') 0
--   where go :: [(Int,Int,Int)] -> Int -> Int
--         go [] result = result
--         go((i,m,o):ys) result = if m == 0
--                                then go ys (result +o)
--                                else
--                                   let changedPart = map (\(a,b,c) -> (a,b,(c+1+o)) ) (take m  ys)
--                                       unchangedPart = drop m ys
--                                   in
--                                   go ((traceShowId changedPart) ++ unchangedPart) (result+m+o)

            

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

a = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"




