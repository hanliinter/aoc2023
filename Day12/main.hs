module Main where
import Control.Monad.State
import qualified Data.Map as Map
import Data.Functor
import Data.List
import Debug.Trace
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'



--solvePart1 :: [String] -> Int
solvePart1 contents = sum $ map process contents
  where process :: String -> Int
        process l = let str = head $ words l
                        nums = map read $ wordsWhile (== ',') $ last $ words l
                    in
                      solve False str nums

solvePart2 contents =  sum $ map process contents
  where process :: String -> Int
        process l = let str = intercalate "?" $ take 5 $ repeat $  head $ words l
                        nums =concat $ take 5 $ repeat $ map read $ wordsWhile (== ',') $ last $ words l
                    in
                      evalState (solve_memo False str nums) Map.empty
solve _ [] [] = 1
solve _ [] [0] = 1
solve _ [c] [x] = case c of
                   '.' -> if x == 0 then 1 else 0
                   '#' -> if x == 1 then 1 else 0
                   '?' -> if x <= 1 then 1 else 0
solve _ [c] [] = case c of
                   '.' -> 1
                   '#' -> 0
                   '?' -> 1                    
                -- if x > 0 then solve cs xs else solve cs rest


solve prev (c:cs) xs | xs == [] = case c of
                                    '.' -> solve False cs []
                                    '#' -> 0
                                    '?' -> solve False cs []
                     | otherwise = let (x:rest) = xs
                                       cond1 = if prev then
                                                 if x == 0  then solve False cs rest else 0
                                               else solve False cs xs
                                       cond2 = if x <= 0 then 0 else solve True cs ((x-1):rest)
                                   in
                                     if (length $ filter (\x -> x =='#' || x == '?') (c:cs)) < sum xs then 0
                                     else
                                       case c of
                                         '?' -> cond1 + cond2 --(traceShowId $ cond1) + (traceShowId $ cond2)
                                         '.' -> cond1
                                         '#' -> cond2
 
solve _ [] xs = 0
--solve prev cs [] = if length cs == 1 then solve prev cs [0] else traceShow ("cs++" ++ cs) $ length cs


type Dict = Map.Map (String, [Int]) Int
solve_memo :: Bool -> String -> [Int] -> State Dict Int
solve_memo _ [] [] = return 1
solve_memo _ [] [0] = return 1
solve_memo _ [c] [x] = case c of
                   '.' -> if x == 0 then return 1 else return 0
                   '#' -> if x == 1 then return 1 else return 0
                   '?' -> if x <= 1 then return 1 else return 0
solve_memo _ [c] [] = case c of
                   '.' -> return 1
                   '#' -> return 0
                   '?' -> return 1                    
                -- if x > 0 then solve cs xs else solve cs rest


solve_memo prev (c:cs) xs | xs == [] = case c of
                                    '.' -> solve_memo False cs []
                                    '#' -> return 0
                                    '?' -> solve_memo False cs []
                     | otherwise = let (x:rest) = xs
                                       cond1 = if prev then
                                                 if x == 0  then gets (Map.lookup (cs,rest)) >>= \result ->
                                                                  case result of
                                                                   Nothing -> solve_memo False cs rest >>= \num -> modify (Map.insert (cs,rest) num) >> return num
                                                                   Just num -> return num
                                                             else return 0
                                               else gets (Map.lookup (cs,xs)) >>= \result ->
                                                    case result of
                                                      Nothing -> solve_memo False cs xs >>= \num -> modify (Map.insert (cs,xs) num) >> return num
                                                      Just num -> return num
                                       cond2 = if x <= 0 then return 0
                                               else gets(Map.lookup (cs, ((x-1):rest))) >>= \result ->
                                                     case result of
                                                       Nothing -> solve_memo True cs ((x-1):rest) >>= \num -> modify (Map.insert (cs,((x-1):rest)) num) >> return num
                                                       Just num -> return num
                                   in
                                     if (length $ filter (\x -> x =='#' || x == '?') (c:cs)) < sum xs then return 0
                                     else
                                       case c of
                                         '?' -> fmap (+) cond1 <*> cond2 
                                         '.' -> cond1
                                         '#' -> cond2
 
solve_memo _ [] xs = return 0
