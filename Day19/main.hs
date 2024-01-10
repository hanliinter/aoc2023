module Main where

import Control.Monad
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Debug.Trace
main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart2 <&> show >>= putStrLn

type Val = String
type OP = Char
data Action = A
            | R
            | JC Val OP Int String
            | JMP String
--            | Compose [Action]

instance Show Action where
  show A = "Accept"
  show R = "Reject"
  show (JC name op i t) = "Jump from " ++ name ++ " to "++ t
  show (JMP name) = "Jump to " ++ name


instance Eq Action where
  (==) A A = True
  (==) R R = True
  (==) _ _ = False


readEndState :: String -> Action
readEndState "A" = A
readEndState "R" = R

type Rules = Map.Map String [Action]

solvePart1 contents = let rulesStr = head $ wordsWhile (=="") contents
                          valuesStr = head $ tail $ wordsWhile (=="") contents
                          rules = Map.fromList $ map parseActions rulesStr
                          values = map parseValues valuesStr
                      in
--                        (map (sum . (map snd)) values,map (execute rules) values)
                       sum $ map fst $ filter ((==True).snd) $ map (go rules) values
             where
               go rules value = ((sum $ map snd value), (execute rules value))


solvePart2 contents = let rulesStr = head $ wordsWhile (=="") contents
                          valuesStr = head $ tail $ wordsWhile (=="") contents
                          rules = Map.insert "R" [R] $ Map.insert "A" [A] $ Map.fromList $ map parseActions rulesStr
                          upLimit = 4000
                          values = [("x",(1,upLimit)),("m",(1,upLimit)),("a",(1,upLimit)),("s",(1,upLimit))]
                      in
--                        (map (sum . (map snd)) values,map (execute rules) values)
                      -- length $ filter (==False)$ map (execute rules) values
                        execute' rules values



wordsWhile :: (a->Bool) -> [a] -> [[a]]
wordsWhile p xs = case dropWhile p xs of
                    [] -> []
                    xs' -> w: wordsWhile p xs''
                     where (w,xs'') = break p xs'

readInt :: String -> Int
readInt = read
readAction :: String -> Action
readAction str = let result = wordsWhile (==':') str
                  in
                   if length result == 1 then JMP $ head result
                   else
                     let condition = head result
                         target = head $ tail result
                         (name:value:[]) = wordsWhile (\x -> x == '<' || x == '>') condition
                         op = head $ filter (\x -> x == '<' || x == '>') condition
                      in
                       JC name op (readInt value)target 

parseActions str = let stubs = words $ map (\x -> if x == '{' || x == '}' then ' ' else x) str
                       name = head $ stubs
                       actions = wordsWhile (==',') $ head $ tail stubs
                   in
                (name,map readAction actions)


parseValues str =  let values = wordsWhile (==',') $ filter (\x ->  x /= '{' && x /= '}' ) str
                   in
                     map go values
       where go str = let name = head $ wordsWhile (=='=') str
                          value = readInt $ head $ tail $ wordsWhile (=='=') str
                       in
                        (name,value)
                      

execute :: Rules -> [(String,Int)] -> Bool
execute rules vs = case Map.lookup "in" rules of
                    Nothing -> error "Should never happen"
                    Just as -> case apply rules as vs of
                                 Left a -> if a == A then True else False
                                 Right _ -> error "should not happen"


getRange :: (Int, Int) -> Int
getRange (a,b) = b - a +1

divide :: (Int,Int) -> Char -> Int -> (Maybe (Int,Int),Maybe(Int,Int))
divide (a,b) op i = if op == '<' then
                      if b <= i then (Just (a,b), Nothing)
                      else if i <= a then (Nothing,Just(a,b)) else (Just (a,i-1),Just (i,b))
                    else
                      if a > i then (Just (a,b),Nothing)
                      else if b <= i then (Nothing,Just (a,b)) else (Just(i+1,b),Just (a,i))
                      


countRange :: [(String,(Int,Int))] -> Int
countRange xs = product $ map (getRange . snd) xs


execute' :: Rules -> [(String,(Int,Int))] -> Int
execute' rules vs =  case Map.lookup "in" rules of
                    Nothing -> error "Should never happen"
                    Just as -> go rules vs as
  where
    go rules vs [] = error $ "should not happen by law!"
    go rules vs (A:_)  = countRange $ traceShowId  vs
    go rules vs (R:_)  = 0
    go rules vs ((JMP name):_) = case Map.lookup name rules of
                                   Nothing -> error $ "could not find the rule:" ++ name
                                   Just as -> go rules vs as
    go rules vs ((JC name op i t):rest) = let tempMap = Map.fromList vs
                                              range = case Map.lookup name tempMap of
                                                        Nothing -> error $ "could not find the value:" ++ name
                                                        Just r -> r
                                              (succHalf,failHalf) = divide range op i
                     
                                          in
                                            case (succHalf, failHalf) of
                                              (Nothing, Nothing) -> error "no way"
                                              (Just succ,Nothing) ->
                                                let vs' = updateList name succ tempMap in
                                                  case Map.lookup t rules of
                                                    Nothing -> error $ "could not find the rule:" ++ name
                                                    Just as' -> go rules vs' as'
                                              (Nothing,Just fail) -> let vs' = updateList name fail tempMap in
                                                                      go rules vs' rest
                                              (Just succ,Just fail) ->
                                                let vs' = updateList name succ tempMap
                                                    vs'' = updateList name fail tempMap
                                                in
                                                  case Map.lookup t rules of
                                                    Nothing -> error "could not find the rule:"
                                                    Just as' ->
                                                      (go rules vs' as') + (go rules vs'' rest)
                                                
    
    
updateList :: String -> (Int,Int) -> Map.Map String (Int,Int) -> [(String,(Int,Int))]
updateList  name range tempMap = Map.toList $ Map.insert name range tempMap



apply :: Rules -> [Action] -> [(String,Int)] -> Either Action [(String,Int)]
apply rules as vs = foldM (go rules) vs as
  where go rules vs a = case a of
                          A -> Left A
                          R -> Left R
                          JC name op vc t -> let f = if op == '<' then (<vc) else (>vc) in 
                                               case lookup name vs of
                                                 Nothing -> error "maybe I should return Reject?"
                                                 Just v -> if f v
                                                           then
                                                             if t == "R" || t == "A" then Left $ readEndState t else
                                                                case Map.lookup t rules of
                                                                  Nothing -> error $ show t ++ "should not happen, but maybe I should return Reject" 
                                                                  Just r -> apply rules r vs
                                                                      
                                                           else Right vs
                          JMP t -> if t == "R" || t == "A" then Left $ readEndState t else
                                     case Map.lookup t rules of
                                     Nothing -> error $ show t ++ "should not happen, but maybe I should return Reject"
                                     Just r -> apply rules r vs
                 
                  -- let JC name f next = a
                  -- in
                  --   case lookup name vs of
                  --        Nothing -> error "Your program has a bug"
                  --        Just v -> 
                  --          if f v then
                  --          case Map.lookup next rules of
                  --            Nothing -> error $ "could not find the next rule:" ++ next
                  --            Just next' -> apply rules next' vs
                  --          else R
                    


--solvePart1 = id -- let rule = Map.insert "in" 
