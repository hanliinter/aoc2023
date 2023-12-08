module Main where
import Data.Char
import Data.Functor
import Debug.Trace
import Control.DeepSeq
wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace




main :: IO ()
main = readFile "input.txt" <&> lines <&> filter (/= "") <&> solvePart2 <&> show >>= putStrLn
solvePart1 :: [String] -> Int
solvePart1 (directions:maps) = let dict = map makeConnection maps
                               in
                                 length $  go (cycle directions) dict ["AAA"]
   where go (x:xs) dict result@(r:rest) = if step dict r x == "ZZZ"
                                          then result
                                          else go xs dict ((step dict r x):result)

makeConnection :: String -> (String, (String,String))
makeConnection str = let source = trim $ head $ wordsWhile (== '=') str
                         targets = filter (\x -> x /= '(' && x /=  ')') $ trim $ last $ wordsWhile (== '=') str
                         left = trim $ head $ wordsWhile (==',') targets
                         right = trim $ last $ wordsWhile (==',') targets
                     in
                       (source, (left,right))


step ::[(String,(String,String))] -> String -> Char -> String
step dict node direction = case lookup node dict of
                             Nothing -> error "could not find"++ node
                             Just (left,right) -> case direction of
                                                    'L' -> left
                                                    'R' -> right



isSource = checkNode (=='A')
isTarget = checkNode (=='Z')
checkNode :: (Char->Bool) ->String -> Bool
checkNode _ [] = error "should be a valid string"
checkNode p xs = p $ last xs

solvePart2 :: [String] -> Int
solvePart2 (directions:maps) = let dict = map makeConnection maps
                                   startList = filter (isSource) $map fst dict 
                               in
                                  go (cycle directions) dict startList 0
   where go (x:xs) dict r result = if all isTarget $ stepN dict r x
                                          then result+1
                                          else go xs dict (stepN dict r x) result+1



stepN ::[(String,(String,String))] -> [String] -> Char -> [String]
stepN dict nodes direction = force $ map (\x -> step dict x direction) nodes
