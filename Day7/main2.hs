module Main where

import Data.Char
import Data.Functor
import Data.List
import Debug.Trace


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn


--solvePart1 :: [String] -> Int  
solvePart2 content = let handsList = map (hands.head) $ map words content
                         betList = map (read. last) $ map words content
                         gameList = zip handsList betList
                     in
                       sum $ zipWith (\n (_,b) -> n * b) [1..] $  sort gameList
  --map createHands $ map words contents

data Card = J |N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T  | Q | K | A deriving(Eq,Ord,Show)

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind| FullHouse | FourOfAKind| FiveOfAKind  deriving (Eq,Ord,Show)

cards = [('A', A),
         ('K', K),
         ('Q', Q),
         ('T', T),
         ('9', N9),
         ('8', N8),
         ('7', N7),
         ('6', N6),
         ('5', N5),
         ('4', N4),
         ('3', N3),
         ('2', N2),
         ('J', J)
         ]         

charToCard c = case lookup c cards of
                 Nothing -> error $ "should not happen" ++ show c
                 Just d -> d



strToHands = map charToCard



handsType :: [Card] -> Type
handsType cards = if (length $ filter (== J) cards) == 0
                  then
                   case sort $ map length $ group $ sort cards of
                    (1:1:1:1:1:_) -> HighCard
                    (1:1:1:2:_)   -> OnePair
                    (1:2:2:_)     -> TwoPair
                    (1:1:3:_)     -> ThreeOfAKind
                    (2:3:_)       -> FullHouse
                    (1:4:_)       -> FourOfAKind
                    (5:_)         -> FiveOfAKind
                    _ -> error $ show cards
                 else
                    case filter (/= J) cards of
                      [] -> FiveOfAKind
                      other -> let t = findTarget other
                               in
                                  handsType $ replace J t cards



replace :: Eq a => a -> a -> [a] -> [a]
replace x y s = go x y s []
  where go :: Eq a => a -> a -> [a] -> [a] -> [a]
        go _ _ [] result = reverse result
        go x y (c:cs) result = if x == c then go x y cs (y:result)
                                         else go x y cs (c:result)


findTarget :: [Card] -> Card
findTarget cs = head $ last $ sortBy (\a b -> length a `compare` length b) $ group $ sort cs
--handsType _ = error "could not decide type"

hands :: String -> (Type, [Card])
hands str = (handsType $ strToHands str, map charToCard str)
