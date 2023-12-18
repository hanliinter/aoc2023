module Main where

import Control.Monad.Fix
import Data.Functor
import Data.List

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'

main :: IO ()
main = readFile "input.txt" <&> lines <&> (\x -> take 100 $ map  calcNorth $ map (\i -> repeat' cycleRocks x i) [1..])  <&> show >>= putStrLn
solve contents = sum $ map (calc .tilt) $ transpose contents

calcNorth contents = sum $ map calc $ transpose contents

--tiltNorth contents = map tilt $ transpose contents

tilt str = concatMap change $ groupBy goChar str

--cycleRocks :: [String] -> 
cycleRocks content = let north' = transpose $ map tilt $ transpose content
                         west' = map tilt north'
                         south' = reverse $ transpose $ map tilt $ transpose $ reverse west'
                         est' = map reverse $ map tilt $ map reverse south'
                     in
                       est'

repeat' f init 0 = init
repeat' f init n = repeat' f (f init) (n -1)

--markPos contents = map (\(j,s) -> map (\(i,c) -> ((i,j),c)) $ zip [1..] s) $ zip [1..] contents                       
--groupBy
goChar :: Char -> Char -> Bool
goChar a b | a == '#' && b == '#' = True
           | a /= '#' && b /= '#' = True
           | otherwise = False

change :: String -> String
change [] = []
change sharp@('#':_) = sharp
change rocks = let r = length $ filter (=='O') rocks
                   d = length $ filter (=='.') rocks
               in
                 (replicate r 'O' ) ++ (replicate d '.')

--calc :: String -> Int
calc str = sum $ map (\(i,_) -> i) $ filter ((=='O').snd) $ zip [1..] $ reverse str
