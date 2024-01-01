module Main where

import Data.Functor
import Data.List

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn



wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'

isSingleAxisChange :: String -> Bool
isSingleAxisChange str = let firstEnd = map read $ wordsWhile (==',') $ head $ wordsWhile (=='~') str
                             secondEnd = map read $ wordsWhile (==',') $ head $ tail $ wordsWhile (=='~') str
                             num_of_zeros = length $ filter (/=0) $ zipWith (-) firstEnd secondEnd
                          in
                           num_of_zeros == 1 || num_of_zeros == 0
solvePart1 contents = filter ((==False).snd) $ map (\x -> (x,isSingleAxisChange x)) contents
