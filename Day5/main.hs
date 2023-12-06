module Main where

import Data.List
import Data.Foldable
import Data.Functor
import Data.Char
import Control.DeepSeq
import Control.Parallel.Strategies
import System.Environment
wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


main :: IO ()
main = getArgs <&> head  >>= readFile  <&> lines <&> solvePart1 <&> show >>= putStrLn
solvePart1 :: [String] -> Int
solvePart1 contents = let (seedsString:s2s:s2f:f2w:w2l:l2t:t2h:_) = wordsWhile (== "") contents
                          seeds = map read $ words $ last $ wordsWhile (== ':') $ head seedsString
                          seedsList = seedsInPart2  seeds
                          rangesStrings = tail $ wordsWhile (== "") contents
                          ranges = map go $ map tail rangesStrings
                          result = map  (map (\x -> foldl convert x ranges )) seedsList
                          result' = result  `using` parBuffer 10000 rseq
                      in
                        minimum $ map minimum result'
                          
                            where
                              go str = map go' str
                                where go' line = let (a:b:c:_) = map read $ words line
                                                 in
                                                   --zip [b..(b+c-1)] [a..]
                                                   (a,b,c)
                          

convert :: Int -> [(Int,Int,Int)] -> Int
convert s dict = case asum $ map (go s) dict of
                   Just c -> c
                   Nothing -> s
  where
    go :: Int -> (Int,Int,Int) -> Maybe Int
    go s (a,b,c) = if (s - b) >= 0 && (s -b) < c then Just (a + (s - b))
                                                    else Nothing



seedsInPart2 :: [Int] -> [[Int]]
seedsInPart2 xs = if length xs `mod` 2 /= 0 then error "reading list error"
                                            else go xs []
                      where go :: [Int] -> [[Int]] -> [[Int]]
                            go [] result = result
                            go (x:y:rest) result = go rest ([x..(x+y-1)]:result) 
