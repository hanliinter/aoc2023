module Main where

import Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Char(isSpace)
import Control.Monad.State
import Data.Maybe
import Debug.Trace
  
main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn

type Nodes = Map.Map String [String]
type NodesNN = Map.Map String Int
type NodesSet = Set.Set (Int,String)

wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


createGraph :: [String] -> Nodes
createGraph contents = foldl' go Map.empty contents
  where go nodes content = let h = head $ wordsWhile (==':') content
                               ts = words $ trim $ head $ tail $ wordsWhile (==':') content
                           in
                              foldl' (go' h) nodes ts
        go' u nodes v = let nodes' = Map.insertWith (++) u [v] nodes
                            nodes'' = Map.insertWith (++) v [u] nodes'
                         in
                          nodes''

createNodesNeiboursCount = Map.map (length) 


count ::  Nodes -> NodesSet-> String -> Int
count nodes nodesSet u = let nodesSet' = Set.map (\(c,n) -> n) nodesSet in
                         length $ filter (\t -> not (t `Set.member` nodesSet')) $ fromJust $ Map.lookup u nodes

countCurrentNeibours ::  Nodes -> NodesSet -> Int
countCurrentNeibours nodes nodesSet = sum $ map (\(c,n) -> count nodes nodesSet n) $ Set.toList nodesSet


countCurrentNeibours' ::  Nodes -> NodesSet -> [(Int,String)]
countCurrentNeibours' nodes nodesSet =  map (\(c,n) -> (count nodes nodesSet n,n)) $ Set.toList nodesSet

removeNodesWithMostNeibours :: NodesSet ->Nodes -> NodesSet
removeNodesWithMostNeibours nodesSet nodes = let nodesSet' = Set.fromList $ countCurrentNeibours' nodes nodesSet
                                              in
                                               Set.deleteMax nodesSet'
                                           


-- solvePart1  contents = let nodes = createGraph contents
--                            nodesNN = createNodesNeiboursCount $ nodes
--                            nodesSet = Set.fromList $ map (\(n,c) -> (c,n)) $ Map.toList nodesNN
--                         in
--                           --count nodes nodesSet "frs"
--                          countCurrentNeibours' nodes nodesSet
solvePart1 contents = let nodes = createGraph contents
                          nodesNN = createNodesNeiboursCount $ nodes
                          nodesSet = Set.fromList $ map (\(n,c) -> (c,n)) $ Map.toList nodesNN
                          n = evalState solve (nodes, nodesSet)
                      in
                        (Set.size nodesSet -n ) * n



-- solve :: State (Nodes,NodesSet) [Int]
-- solve = do
--    (nodes,nodesSet) <- get
--    if  countCurrentNeibours nodes nodesSet /= 3
--     then return $ countCurrentNeibours' nodes nodesSet
--     else
--        do
--        let nodesSet' = Set.deleteMax $ traceShowId nodesSet
--        put (nodes,nodesSet')
--        solve
solve :: State (Nodes,NodesSet) Int
solve = do
  (nodes,nodesSet) <- get
  if  countCurrentNeibours nodes nodesSet == 3 || Set.size nodesSet == 0
    then return $ Set.size nodesSet
    else
       do
       let nodesSet' = removeNodesWithMostNeibours nodesSet nodes
       put (nodes, nodesSet')
       solve
  

-- learn this from /u/4HbQ

