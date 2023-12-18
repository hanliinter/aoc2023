{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Function
import Data.Functor
import Data.List
import Debug.Trace
import Data.Bits

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'



main = readFile "input.txt" <&> lines <&>  solvePart2 <&> show >>= putStrLn
data Direction = H Int | V Int deriving (Show)

solvePart1 content = sum $ map (\case {H i -> i; V i -> i * 100} ) $ map solve $ wordsWhile (== []) content
solvePart2 content = sum $ map (\case {H i -> i; V i -> i * 100} ) $ map solve' $ wordsWhile (== []) content
--solvePart2' content = map solve' $ wordsWhile (== []) content

--solve :: [String] -> 
solve content = let cols = getFlipPoint $ zip [1..] $ map toWord $ transpose content
                    rows = getFlipPoint $ zip [1..] $ map toWord $ content
                    in case (cols, rows) of
                         ([],[]) -> error "wrong input"
                         ([],((i,True):_)) -> V i
                         ((i,True):_, []) -> H i
                         (_,_) -> error "find both horizontal & vertical flip point"


solve' content = let cols = getSmudgePoint $ zip [1..] $ map toWord $ transpose content
                     rows = getSmudgePoint $ zip [1..] $ map toWord $ content
                  in 
                     case (cols, rows) of
                         ([],[]) -> error "wrong input"
                         ([],((i,True):_)) -> V i
                         ((i,True):_, []) -> H i
                         (_,_) -> error "find both horizontal & vertical flip point"



toWord :: String -> Int
toWord str = let n = length str
              in
               go str (n-1) 0
 where
   go [] _ result = result
   go (x:xs) n result = case x of
                          '.' -> go xs (n-1) result
                          '#' -> go xs (n-1) (result + 2 ^ n)


canFlip :: [Int] -> Int -> Bool
canFlip xs i | i >= length xs || i <= 0 = False
                    | otherwise =   let pre = reverse $ take i xs
                                        suff = drop i xs
                                    in
                                      all (== 0) $ zipWith (-) pre suff


canFlipWithSmudge :: [Int] -> Int -> Bool
canFlipWithSmudge xs i | i >= length xs || i <= 0 = False
                    | otherwise =   let pre = reverse $ take i xs
                                        suff = drop i xs
                                        l = length $ filter (/= 0) $ zipWith (xor) pre suff
                                        diff = s head $ filter (/= 0) $ zipWith (xor) pre suff
                                    in
                                     if l /= 1 then False
                                     else if isExpOfTwo diff then True else False




-- xs : [(i,val)] , i start from 1
getFlipPoint xs = let vals = map snd xs
                     in
                      filter ((==True).snd) $ map (\(i,v) -> (i,canFlip vals i)) xs


getSmudgePoint xs = let vals = map snd xs
                     in
                      filter ((==True).snd) $ map (\(i,v) -> (i,canFlipWithSmudge vals i)) xs



toBin :: Int -> [Int]
toBin n | n <0 = []
        | n == 0 = [0]
        | otherwise = go n []
  where go 0 result = result
        go n result = go (n `div` 2) ((n `mod` 2):result)

isExpOfTwo :: Int -> Bool
isExpOfTwo n = let r = toBin n
               in
                 (head r == 1) && all (==0)  (tail r)

toPos :: Int -> Int
toPos n = length $ toBin n
