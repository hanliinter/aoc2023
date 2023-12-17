{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Function
import Data.Functor
import Data.List



wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'



main = readFile "input.txt" <&> lines <&>  solvePart1 <&> show >>= putStrLn


data Direction = H Int | V Int deriving (Show)

solvePart1 content = sum $ map (\case {H i -> i; V i -> i * 100} ) $ map solve $ wordsWhile (== []) content


--solve :: [String] -> 
solve content = let cols = getFlipPoint $ zip [1..] $ map toWord $ transpose content
                    rows = getFlipPoint $ zip [1..] $ map toWord $ content
                    in case (cols, rows) of
                         ([],[]) -> error "wrong input"
                         ([],((i,True):_)) -> V i
                         ((i,True):_, []) -> H i
                         (_,_) -> error "find both horizontal & vertical flip point"

slice :: Int -> Int -> String -> String
slice start end str = take (end - start) $ drop start str


-- findLongestParlindomeAt :: String -> Int -> Int
-- findLongestParlindomeAt str i = let n = length str
--                                     rad = if n-i > i then n-i else i
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

-- xs : [(i,val)] , i start from 1
getFlipPoint xs = let vals = map snd xs
                     in
                      filter ((==True).snd) $ map (\(i,v) -> (i,canFlip vals i)) xs
