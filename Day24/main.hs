module Main where

import Data.Functor
import Data.List
import Data.Char(isSpace)
import Control.Monad

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace



data Ray = PV (Int,Int,Int) (Int,Int,Int) deriving (Show,Eq,Ord)

data Line = Line Int Int Int deriving (Show,Eq,Ord)  -- Ax + By = C           

readRay :: String -> Ray
readRay str = let pointStr = trim $ head $ wordsWhile (=='@') str
                  vecStr   = trim $ head $ tail $ wordsWhile (=='@') str
                  (x:y:z:[]) = map read $ wordsWhile (==',') pointStr
                  (dx:dy:dz:[]) = map read $ wordsWhile (==',') vecStr
               in
                PV (x,y,z) (dx,dy,dz)

lineOf :: Ray -> Line
lineOf (PV (px,py,pz) (dx,dy,dz)) = let a = dy
                                        b = -dx
                                        c = dy * px - dx * py
                                    in
                                    Line a b c


isParallel :: Line -> Line -> Bool
isParallel (Line a1 b1 c1) (Line a2 b2 c2) = a2 * b1 == a1 * b2

type Point = (Float,Float)
--findIntersection :: Line -> Line -> Point
findIntersection (Line a1 b1 c1) (Line a2 b2 c2) = let a1' = fromIntegral a1 :: Float
                                                       b1' = fromIntegral b1 :: Float
                                                       c1' = fromIntegral c1 :: Float
                                                       a2' = fromIntegral a2 :: Float
                                                       b2' = fromIntegral b2 :: Float
                                                       c2' = fromIntegral c2 :: Float

                                                       x =  (c1' * b2' - c2' * b1')/ (a1' * b2' - a2' * b1')
                                                       y = (c2' * a1' - c1' * a2') / (a1' * b2' - a2' * b1')
                                                    in
                                                     (x,y)


main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>=  putStrLn

minVal = 200000000000000
maxVal = 400000000000000
--minVal = 7
--maxVal = 27

pairs xs = [(x,y) | x<-xs, y<-xs, x <y]
--solvepart1 :: [String] ->
-- solvePart1 = findInterPoints
findInterPoints contents = let  rays = map readRay contents
                                lines = zip rays $ map lineOf $ rays
                           in
                             do
                               (a,b) <- pairs lines
                               let r1@(PV (px1,py1,_) (dx1,dy1,_)) = fst a
                               let r2@(PV (px2,py2,_) (dx2,dy2,_)) = fst b
                               let l1 = snd a
                               let l2 = snd b
                               guard (not (isParallel (snd a) (snd b)))
                               let (x,y) = findIntersection (snd a) (snd b)
                               guard (all(\n -> n >= minVal && n <= maxVal) [x,y])
                               guard ((x- fromIntegral px1) * fromIntegral dx1 >= 0 && (y- fromIntegral py1) * fromIntegral dy1 >= 0)
                               guard ((x- fromIntegral px2) * fromIntegral dx2 >= 0 && (y- fromIntegral py2) * fromIntegral dy2 >= 0)
                               return $ (l1,l2)



solvePart1 =  length . findInterPoints
-- findInterPoints contents = let  rays = map readRay contents
--                                 lines = zip rays $ map lineOf $ rays
--                            in
--                              do
--                                (a,b) <- pairs lines
--                                guard (not (isParallel (snd a) (snd b)))
--                                let (x,y) = findIntersection (snd a) (snd b)
                               
--                                return (a,b)



