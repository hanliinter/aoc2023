module Main where

import Data.Functor
import Data.Function
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn
type Endpoint = (Int,Int,Int)


-- a map that records the bricks 'support' the given brick
type SMap = Map.Map (Int,Brick) [Brick]

data Brick = Brick Endpoint Endpoint deriving (Show, Eq)


instance Ord Brick where
  (<=) :: Brick -> Brick -> Bool
  (<=) (Brick _ (_,_,z1)) (Brick _ (_,_,z2)) = z1 <= z2

readBrick :: String -> Brick
readBrick str = let firstEnd = map read $ wordsWhile (==',') $ head $ wordsWhile (=='~') str
                    secondEnd = map read $ wordsWhile (==',') $ head $ tail $ wordsWhile (=='~') str
                    (x1:y1:z1:[]) = firstEnd
                    (x2:y2:z2:[]) = secondEnd
                 in
                  Brick (x1,y1,z1) (x2,y2,z2)


downTo :: Int -> Brick -> Brick
downTo n (Brick (x1,y1,z1) (x2,y2,z2)) = (Brick (x1,y1,n) (x2,y2,z2-(z1-n) )) 
-- learned this overlap function from HyperNeutrino
overlapBrick :: Brick -> Brick -> Bool
overlapBrick (Brick (x1,y1,z1) (x2,y2,z2)) (Brick (x1',y1',z1') (x2',y2',z2')) = overlap (x1,x2) (x1',x2') (y1,y2) (y1',y2')

overlap :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
overlap (x1s,x1e) (x2s,x2e) (y1s,y1e) (y2s,y2e) = (max x1s x2s) <= (min x1e x2e) && (max y1s y2s) <= (min y1e y2e) 

isSupporting :: Brick -> Brick -> Bool
isSupporting brickA brickB = overlapBrick brickA brickB && getHighZ brickA +1 == getLowZ brickB

getLowZ (Brick (_,_,z1) (_,_,z2)) = min z1 z2
getHighZ (Brick (_,_,z1) (_,_,z2)) = max z1 z2

fallDown :: [Brick] -> [Brick]
fallDown bricks = let  
                       comp = compare `on` getLowZ
                       sorted = sortBy comp bricks
                    in
                    
                     foldl go [] sorted
      where 
            go :: [Brick] -> Brick -> [Brick]
            go [] current = [downTo 1 current]
            go stable current  = let l = filter (overlapBrick current) stable
                                          in
                                            case l of
                                              [] -> insert (downTo 1 current) stable
                                              stable' -> let (h:_) = reverse stable'
                                                             in
                                                           insert (downTo ((getHighZ h) +1) current) stable
            insert :: Brick -> [Brick] -> [Brick]
            insert x [] = [x]
            insert x (y:ys) = if x <=y
                              then x:y:ys
                              else y: insert x ys



markBrick :: [Brick] -> Map.Map Brick Int
markBrick bricks = Map.fromList $ zip bricks [0..]
            
findSupportingBelow :: [Brick] -> (Map.Map Brick [Brick],Map.Map Brick [Brick])
findSupportingBelow bricks = let support = Map.fromList $ map (\x -> (x,[])) bricks in
                                   go [] bricks (Map.empty,Map.empty)
  where go prev [] (dict,support) = (dict,support)
        go prev (a:rest) (dict,support)  = let --(i,a') = a
                                     cs = filter (`isSupporting` a) prev
                                     dict' = Map.insert a cs dict
                                     support' = foldl' (prepare a) support cs
                                  in
                                   go (a:prev) rest (dict', support')
        prepare a support c = Map.insertWith (++) c [a] support


wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'

isSingleAxisChange :: String -> Bool
isSingleAxisChange str = let firstEnd = map read $ wordsWhile (==',') $ head $ wordsWhile (=='~') str
                             secondEnd = map read $ wordsWhile (==',') $ head $ tail $ wordsWhile (=='~') str
                             num_of_zeros = length $ filter (/=0) $ zipWith (-) firstEnd secondEnd
                          in
                           num_of_zeros == 1 || num_of_zeros == 0



solvePart1 contents = let bricks =  map (readBrick) contents
                          
                          comp = (flip compare) `on` getLowZ
                          (supportBy, support) =  findSupportingBelow $ fallDown bricks
                          
                      in
                        --markBrick $ fallDown bricks
                       --supportBy
                      -- support
                       length $ nub $ sort $ filter (\b -> go b supportBy support) $ fallDown bricks
                       
                       
                       -- lookup brick  (Map.toList supportBy)
    where go b supportBy support = case lookup b (Map.toList $ support) of
                                     Nothing -> True -- error $ "should never happen" ++ show b ++ (show $ Map.toList support)
                                     Just c -> case c of
                                       [] -> True
                                       xs -> all (\b' -> go' b' supportBy) xs
          go' b supportBy  = case lookup b (Map.toList supportBy) of
                                      Nothing -> error $ "Should never happen" ++ show b ++ "\n" ++ show supportBy
                                      Just c -> case c of
                                        [] -> error "also should never happen"
                                        cs -> length cs > 1
