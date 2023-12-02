{-# LANGUAGE BlockArguments #-}
import Data.Char (isSpace)
import Data.List


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


-- RGB
data RGB = R Int
         | G Int
         | B Int
           deriving (Eq,Ord, Show)


readNumber :: String -> String -> RGB
readNumber num name = case name of
                        "red" -> R (read num)
                        "green" -> G (read num)
                        "blue" -> B (read num)

absorb ::  (Int -> Int -> Bool) -> (RGB, RGB,RGB) -> RGB -> (RGB,RGB,RGB)
absorb f ((R r), (G g), (B b)) c = case c of
                               R r' -> (R (if f r r' then r else r'), G g, B b)
                               G g' -> (R r, G (if f g g' then g else g'), B b)
                               B b' -> (R r, G g, B (if f b b' then b else b'))                               




comp :: (RGB, RGB, RGB) -> (RGB,RGB,RGB) -> Bool
comp (r,g,b) (r',g',b') = r <= r' && g <= g' && b <= b'


calcNumber :: String -> Bool
calcNumber str = let sets = wordsWhile (== ';') (trim str)
                     cubes = concatMap (wordsWhile (== ',') .trim) sets
                     formalized =map ((\c -> let a = head c
                                                 b = last c
                                  in
                                   readNumber a b) . wordsWhile (== ' ')) cubes
                     maxNumber = foldl (absorb (<)) ((R 0), (G 0), (B 0)) formalized
                  in
                     comp maxNumber ((R 12), (G 13), (B 14))


calcNumber2 :: String -> Int
calcNumber2 str = let sets = wordsWhile (== ';') (trim str)
                      cubes = concatMap (wordsWhile (== ',') .trim) sets
                      formalized =map ((\c -> let a = head c
                                                  b = last c
                                  in
                                   readNumber a b) . wordsWhile (== ' ')) cubes
                      ((R r), (G g), (B b)) = foldl (absorb (>)) ((R 0), (G 0), (B 0)) formalized
                  in
                     product [r,g, b]



solvePart1 :: [String] -> Int
solvePart1 str = sum $ map (\(c, _) -> c) $ filter (\(_, r) -> r == True) $ map parse str

  where parse str = let  gameStr = wordsWhile (== ':') str
                         gameID = read $ last $ wordsWhile (== ' ') (head gameStr)
                         gameLog = last gameStr
                         in
                      (gameID, calcNumber gameLog)



solvePart2 :: [String] -> Int
solvePart2 str = sum $ map (\(i,c ) -> c)  $ map parse str

  where
    parse :: String -> (Int, Int)
    parse str = let  gameStr = wordsWhile (== ':') str
                     gameID = read $ last $ wordsWhile (== ' ') (head gameStr)
                     gameLog = last gameStr
                         in
                      (gameID, (calcNumber2 gameLog))


  

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ show $ solvePart2 $ lines content
