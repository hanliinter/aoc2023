module Main where
import Data.Char
import Data.Containers.ListUtils
import Data.List
import Data.Maybe
main :: IO ()
main = lines <$> readFile "input.txt" >>= solvePart2


solvePart1 :: [String] -> IO ()
solvePart1 contents = let symbolsPos =  getSymbol $ markPos contents
                          targetPos = nubOrd $ sort $ concatMap (\(p,_) -> getPos p) symbolsPos
                          result = sum $ map snd $ nubOrd $ sort $ catMaybes $ map (\p -> findNumber p contents) targetPos
                      in
                        putStrLn $ show result



solvePart2 :: [String] -> IO ()
solvePart2 contents = let symbolsPos =  getEngine $ markPos contents
                          targetPos =  map (\(p,_) -> getPos p) symbolsPos
                          result = sum $ map (product . map snd) $ filter (\a -> length a == 2) $ map (nubOrd . sort . catMaybes) $ map (\ps -> (map (\p -> findNumber p contents) ps)) targetPos
                      in
                        putStrLn $ show result




markPos :: [String] -> [((Int,Int),Char)]
markPos strs = let m = length strs
                   n = length (head strs)
               in
                 concat $ zipWith
                   (\j xs -> map (\(i,c) -> ((i,j),c)) xs)
                   [0..(m-1)]
                   (map (zip [0..(n-1)]) strs)


getSymbol = filter (\(_,c) -> (not $ isDigit c) && c /= '.')

getEngine = filter ((== '*').snd)

findNumber :: (Int,Int) -> [String] -> Maybe ((Int,Int),Int)
findNumber (x,y) str = let line = str !! y
                       in
                         go x line >>= \(s,p) -> Just ((p,y), s)
                       

  where
    go:: Int -> String -> Maybe (Int,Int)
    go n str | n < 0  = Nothing
         | n > (length str -1) = Nothing
         | not (isDigit (str !! n)) = Nothing
         | otherwise = let (pre,c:suff) = splitAt n str
                           prefix = reverse $ takeWhile(isDigit) (reverse pre)
                           suffix = takeWhile(isDigit) suff
                           
                       in
                         Just (read $ prefix ++ c : suffix, n - length prefix)


getPos :: (Int,Int) -> [(Int,Int)]
getPos (x,y) = [(i,j)| i <- [x-1..x+1], j<- [y-1..y+1], (i,j) /= (x,y)]
