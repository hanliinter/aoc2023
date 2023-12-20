module Main where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor
import Data.List (nub,sort)
main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn
solvePart1 contents = let grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents
                          m = length $ head contents
                          n = length contents
                      in
                        length $ nub $ sort $ map fst $ solve grid m n
                        --Map.fromList $ concat $ map (\(j,s) -> map (\(i,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents



solvePart' contents = let grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents
                          m = length $ head contents
                          n = length contents
                      in
                         length  $ nub $ sort $ map fst $ solve' ((0,3),D) grid m n
                        --Map.fromList $ concat $ map (\(j,s) -> map (\(i,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents



solvePart2 contents = let grid = Map.fromList $ concatMap (\(i,s) -> map (\(j,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents
                          m = length $ head contents
                          n = length contents
                          topRow = zipWith (\i j -> ((i,j),D))  (repeat 0) [0..(m-1)]
                          buttomRow = zipWith (\i j -> ((i,j),U)) (repeat (n-1)) [0..(m-1)] 
                          leftCol = zipWith (\i j -> ((i,j),R))   [0..(n-1)] (repeat 0)
                          rightCol = zipWith (\i j -> ((i,j),L))  [0..(n-1)] (repeat (m-1))
                      in
                        --map (\x -> (x,go m n grid x)) $ concat [topRow, buttomRow,leftCol,rightCol]
                       maximum $ map (go m n grid) $ concat [topRow, buttomRow,leftCol,rightCol]
                        --topRow
                        --map (go m n grid) topRow
             where
               go m n grid init = length $ nub $ sort $ map fst $ solve' init grid m n
                        --
                        --Map.fromList $ concat $ map (\(j,s) -> map (\(i,c)-> ((i,j),c)) $ zip [0..] s) $ zip [0..] contents



type Pos = (Int,Int)
type Queue a = ([a],[a])
type Grid = Map.Map Pos Char
type Visited = Set.Set (Pos,Direction)

push :: Eq a => Queue a -> a -> Queue a
push (a,b) n = (a, (n:b))

pushList :: Eq a => Queue a -> [a] -> Queue a
pushList (a,b) b' = (a, reverse b' ++ b)

pop :: Eq a => Queue a-> (a,Queue a)
pop (a,b) = if a == [] then (head $ reverse b,((tail $ reverse b),[] ))
                       else (head a, (tail a, b))


isEmpty :: Eq a => Queue a -> Bool
isEmpty (a,b) = a == [] && b == []

singleton :: Eq a => a -> Queue a
singleton a = ([a],[])


data Direction = U | R | D | L deriving (Eq,Show,Ord)

solve grid m n = go m n (singleton((0,0), R)) grid Set.empty []
 where 
       go m n queue grid visited result | isEmpty queue = result
                                        | otherwise = let (x, rest) = pop queue in
                                                        if Set.member x visited
                                                        then go m n rest grid visited result
                                                        else let visited' = Set.insert x visited
                                                                 p = fst x
                                                                 c = case Map.lookup p grid of
                                                                     Nothing -> error "should not happen"
                                                                     Just c  -> c
                                                                 targets' = turn (snd x) c p m n
                                                                 queue' = pushList queue targets'
                                                                 result' = (x:result)
                                                             in
                                                               go m n queue' grid visited' result'


solve' init grid m n = go m n (singleton(init)) grid Set.empty []
 where 
       go m n queue grid visited result | isEmpty queue = result
                                        | otherwise = let (x, rest) = pop queue in
                                                        if Set.member x visited
                                                        then go m n rest grid visited result
                                                        else let visited' = Set.insert x visited
                                                                 p = fst x
                                                                 c = case Map.lookup p grid of
                                                                     Nothing -> error "should not happen"
                                                                     Just c  -> c
                                                                 targets' = turn (snd x) c p m n
                                                                 queue' = pushList queue targets'
                                                                 result' = (x:result)
                                                             in
                                                               go m n queue' grid visited' result'
                                                               
                                                
                                                

turn :: Direction -> Char -> Pos -> Int -> Int -> [(Pos,Direction)]
turn d '.' p m n = getNext d p m n
turn U '|' p m n = getNext U p m n
turn D '|' p m n = getNext D p m n
turn R '-' p m n = getNext R p m n
turn L '-' p m n = getNext L p m n

turn R '/' p m n = getNext U p m n
turn L '/' p m n = getNext D p m n
turn U '/' p m n = getNext R p m n
turn D '/' p m n = getNext L p m n

turn R '\\' p m n = getNext D p m n
turn L '\\' p m n = getNext U p m n
turn U '\\' p m n = getNext L p m n
turn D '\\' p m n = getNext R p m n


turn R '|' p m n = concat [getNext U p m n, getNext D p m n]
turn L '|' p m n = concat [getNext U p m n, getNext D p m n]
turn D '-' p m n = concat [getNext R p m n, getNext L p m n]
turn U '-' p m n = concat [getNext R p m n, getNext L p m n]

getNext :: Direction -> Pos -> Int -> Int -> [(Pos, Direction)]
getNext d (i,j) m n = case d of
                        U -> if i -1 >= 0 then [((i-1,j),U)]    else []
                        D -> if i +1 < n then  [((i+1, j), D)] else []
                        R -> if j + 1 < m then [((i, j+1), R)] else []
                        L -> if j -1 >= 0 then [((i, j-1), L)] else []                          
