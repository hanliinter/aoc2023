module Main where
import Data.Char
import Data.Functor
wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace




main :: IO ()
main = readFile "sample3.txt" <&> lines <&> filter (/= "") <&> solvePart1' <&> show >>= putStrLn
solvePart1 :: [String] -> Int
solvePart1 (directions:maps) = let dict = map makeConnection maps
                               in
                                 length $  go (cycle directions) dict ["AAA"]
   where go (x:xs) dict result@(r:rest) = if step dict r x == "ZZZ"
                                          then result
                                          else go xs dict ((step dict r x):result)



solvePart1' :: [String] -> Int
solvePart1' (directions:maps) = let dict = map makeConnection maps
                                    startList = filter (isTarget) $ map fst $  map makeConnection maps
                                    startList' = ["FPZ","ZZZ","FNZ","PKZ", "DPZ","TVZ"]
                                    direct = cycle directions
                               in
                                 foldr (lcm) 1 $ map (\x -> go direct dict 0 x) startList
  where    go ::String -> [(String,(String,String))] -> Int -> String -> Int
           go (x:xs) dict result node = if isTarget $ step dict node x 
                                          then result +1
                                          else go xs dict (result+1) (step dict node x)


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

