module Main where

import Data.Functor
import Data.List
import Data.Char(isSpace)
import Control.Monad
import Data.Ratio

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
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>=  putStrLn

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



-- part2 using Gaussian Elimination from https://github.com/shybovycha/gauss-elimination/blob/master/src/Gauss.hs
type Row = [Rational]
type Matrix = [Row]

quicksort :: (Ord a) => [a] -> (a -> a -> Int) -> [a]
quicksort [] _ = []
quicksort (x : xs) cmp = (quicksort lesser cmp) ++ [x] ++ (quicksort greater cmp)
  where
    lesser = [i | i <- xs, (cmp x i) < 0]
    greater = [i | i <- xs, (cmp x i) >= 0]

leadingZeros :: Row -> Int
leadingZeros = length . takeWhile (== 0)

gaussCompareRows :: Row -> Row -> Int
gaussCompareRows r1 r2 = leadingZeros r2 - leadingZeros r1

gaussSortMatrix :: Matrix -> Matrix
gaussSortMatrix = flip quicksort gaussCompareRows

-- r1 has less leading zeros than r2
gaussMakeZero :: Row -> Row -> Row
gaussMakeZero r1 r2 = zipWith (\r1_elt r2_elt -> (r1_elt * factor) + r2_elt) r1 r2
  where
    index = leadingZeros r1
    r1_head = r1 !! index
    r2_head = r2 !! index
    factor = (-1 * r2_head) / r1_head


-- apply the "zeroing head" operation to all the rows except the first one.
-- do this recursively for every row
gaussReduce :: Matrix -> Matrix
gaussReduce [] = []
gaussReduce (r1 : rs) = r1 : gaussReduce (map (gaussMakeZero r1) rs)


gaussFixCoefficients :: Matrix -> Matrix
gaussFixCoefficients [] = []
gaussFixCoefficients (r : rs) = map (/ factor) r : gaussFixCoefficients rs
  where
    index = leadingZeros r
    factor = r !! index

mat :: Matrix
mat = [[1,2,3],[2,6,2]]

gaussSolveMatrix :: Matrix -> Matrix
gaussSolveMatrix mat = mat3
  where
    mat1 = gaussReduce mat
    mat2 = gaussReduce $ reverse mat1
    mat3 = gaussSortMatrix $ gaussFixCoefficients $ reverse mat2


pickItems :: [Ray] -> Matrix
pickItems (h:rays) = concatMap (go h) (take 3 rays)
  where go h r = let PV (x0',y0',z0') (vx0',vy0',vz0') = h
                     PV (xi',yi',zi') (vxi',vyi',vzi') = r
                     x0 = fromIntegral x0'
                     y0 = fromIntegral y0'
                     z0 = fromIntegral z0'
                     vx0 = fromIntegral vx0'
                     vy0 = fromIntegral vy0'
                     vz0 = fromIntegral vz0'
                     xi = fromIntegral xi'
                     yi = fromIntegral yi'
                     zi = fromIntegral zi'
                     vxi = fromIntegral vxi'
                     vyi = fromIntegral vyi'
                     vzi = fromIntegral vzi'
                     
                 in
                   [[vy0 - vyi, vxi - vx0, 0, yi - y0, x0 - xi, 0, x0 * vy0 - y0*vx0 - xi * vyi + yi * vxi],
                   [vz0 - vzi, 0, vxi - vx0, zi - z0,  0, x0 - xi, x0 * vz0 - z0*vx0 - xi * vzi + zi * vxi]]
                   
toInt :: Rational -> Int
toInt r = fromIntegral $ numerator r
getResult :: Matrix -> Int
getResult matrix =  sum $ map (toInt .last) $ take 3 matrix

solvePart2 contents = let  rays = map readRay contents
                           lines = zip rays $ map lineOf $ rays
                       in
                        getResult $ gaussSolveMatrix $ pickItems rays
