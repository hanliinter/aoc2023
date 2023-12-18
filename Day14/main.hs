{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Fix
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Control.Monad.State

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 1000000000 <&> show >>= putStrLn
solvePart2 :: Int -> [String] -> Int
solvePart2 i content = evalState (findCycle content i) (Map.empty, Map.empty)
                           

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

type M = (Map.Map [Int] Int, Map.Map Int Int) -- the matrix and its position
findCycle :: [String] -> Int -> State M Int
findCycle content n = go content n 0
    where go :: [String] -> Int -> Int -> State M Int
          go content n i = if i == n then return (calcNorth content)
                           else
                                gets((Map.lookup  (toWords content)).fst)  >>=
                                                  \case  {Nothing -> modify (\(m,m') -> ((Map.insert (toWords content) i m),m'))
                                                                  >> modify (\(m,m') -> (m,(Map.insert i (calcNorth content) m')))
                                                                  >> go (cycleRocks content) n (i+1)
                                                          ;
                                                          Just s -> let period = i - s
                                                                        reminder = (n - s) `mod` period
                                                                        i' = s + reminder
                                                                    in
                                                                      gets (\(_,m) -> Map.lookup i' m) >>=
                                                         \case { Nothing -> error "Shoult not happen";
                                                                 Just v  -> return v
                                                               }
                                                           
                                                         }


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



toWord :: String -> Int
toWord str = let n = length str
              in
               go str (n-1) 0
 where
   go [] _ result = result
   go (x:xs) n result = case x of
                          '.' -> go xs (n-1) result
                          '#' -> go xs (n-1) result                          
                          'O' -> go xs (n-1) (result + 2 ^ n)

toWords = map toWord
