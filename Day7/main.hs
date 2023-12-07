module Main where

import Data.Char
import Data.Functor
import Data.List
-- wordsWhile :: (a->Bool) -> [a] -> [[a]]
-- wordsWhile p xs = case dropWhile p xs of
--                     [] -> []
--                     xs' -> w: wordsWhile p xs''
--                      where (w,xs'') = break p xs'



trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn


--solvePart1 :: [String] -> Int  
solvePart1 content = let handsList = map (hands.head) $ map words content
                         betList = map (read. last) $ map words content
                         gameList = zip handsList betList
                     in
                       sum $ zipWith (\n (_,b) -> n * b) [1..] $ sort gameList
  --map createHands $ map words contents

data Card = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A deriving(Eq,Ord,Show)

data Type = HighCard | OnePair | TwoPair | ThreeOfAKind| FullHouse | FourOfAKind| FiveOfAKind  deriving (Eq,Ord,Show)

cards = [('A', A),
         ('K', K),
         ('Q', Q),
         ('J', J),
         ('T', T),
         ('9', N9),
         ('8', N8),
         ('7', N7),
         ('6', N6),
         ('5', N5),
         ('4', N4),
         ('3', N3),
         ('2', N2)
         ]         

charToCard c = case lookup c cards of
                 Nothing -> error $ "should not happen" ++ show c
                 Just d -> d



handsType :: String -> Type
handsType cards = case sort $ map length $ group $ sort $ map charToCard cards of
                    (1:1:1:1:1:_) -> HighCard
                    (1:1:1:2:_)   -> OnePair
                    (1:2:2:_)     -> TwoPair
                    (1:1:3:_)     -> ThreeOfAKind
                    (2:3:_)       -> FullHouse
                    (1:4:_)       -> FourOfAKind
                    (5:_)         -> FiveOfAKind
                    _ -> error cards
--handsType _ = error "could not decide type"

hands :: String -> (Type, [Card])
hands str = (handsType str, map charToCard str)
