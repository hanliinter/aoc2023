module Main where
import Data.Char
import Data.Functor
import Debug.Trace


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


main :: IO ()
main = readFile "input.txt" <&> solvePart1 <&> show >>= putStrLn

--solvePart1 :: String -> Int
solvePart1 s = sum $ map hash $ wordsWhile (==',') $ head $ lines s
hash :: String -> Int
hash = foldl go 0
  where go  current c = (17 * (current + (ord c))) `mod` 256


