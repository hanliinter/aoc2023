module Main where

import Data.Functor
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.State

data Module = Flip_Flop Status [String]
            | Conjunction (Map.Map String Pulse) [String]
            | Button
            | Broadcaster [String]
            | Output
            deriving (Show, Eq)


--data ModuleType = Flip_Flop | Conjunction | Button 

data Pulse = High | Low deriving (Show,Eq)
data Status = On | Off deriving (Show,Eq)
data Event = Event String Pulse String deriving (Show,Eq)
type MachineStatus = (Map.Map String Module,Queue Event,(Int,Int),Int)

type Machine a = State MachineStatus a


type Queue a = ([a],[a])

push :: Eq a => Queue a -> a -> Queue a
push (a,b) n = (a, (n:b))

pushList :: Eq a => Queue a -> [a] -> Queue a
pushList (a,b) b' = (a, reverse b' ++ b)

pop :: Eq a => Queue a-> (a,Queue a)
pop (a,b) = if a == [] then (head $ reverse b,((tail $ reverse b),[] ))
                       else (head a, (tail a, b))


empty::Eq a => Queue a
empty = ([],[])

isEmpty :: Eq a => Queue a -> Bool
isEmpty (a,b) = a == [] && b == []

singleton :: Eq a => a -> Queue a
singleton a = ([a],[])

lookup_exn :: (Eq k, Show k, Show v ) => k -> [(k,v)] -> v
lookup_exn k l =   case lookup k l of
                    Nothing -> error $ "should not happen, could not find " ++ show k ++ "in " ++ show l
                    Just v -> v


countEventPulse :: Event -> (Int,Int)
countEventPulse (Event _ p _) = if p == High then (0,1) else (1,0)

--countPulses :: [Event] -> (Int,Int)
isInitial :: Module -> Bool
isInitial Button = True
isInitial (Broadcaster _) = True
isInitial Output = True
isInitial (Flip_Flop s _) = s == Off
isInitial (Conjunction m _) = all (== Low) $ map snd $ Map.toList m

sendAll :: String -> [String] -> Pulse -> [Event]
sendAll from targets p = map (\n -> Event from p n) targets

process :: String -> Module -> (String,Pulse) -> (Module,Maybe [Event])
process name (Flip_Flop status target) (from,p) = if p == High then ((Flip_Flop status target),Nothing)
                                                  else if status == On then ((Flip_Flop Off target),Just (sendAll name target Low))
                                                       else (((Flip_Flop On target),Just (sendAll name target High)))

process name Button _ = (Button, Just $ sendAll name ["broadcast"] Low)
process name Output _ = (Output, Nothing)
process name (Broadcaster target) (from,p) = (Broadcaster target, Just $ sendAll name target p)
process name (Conjunction inputStatus target) (from,p) = let inputStatus' = Map.insert from p inputStatus in
                                                           if all (==High) $ map snd $ Map.toList inputStatus'
                                                           then ((Conjunction inputStatus' target),Just (sendAll name target Low))
                                                           else ((Conjunction inputStatus' target),Just (sendAll name target High))                                                           
  
handlePulse :: Machine ()
handlePulse = do
  (dict,queue,counts@(low,high),buttonPressed) <- get
  if isEmpty queue then
    if all (isInitial) $ map snd $ Map.toList dict then return ()
    else put (dict,singleton (Event "button" Low "broadcaster"), counts,(buttonPressed +1) ) >> handlePulse
  else
    let (e,queue') = pop queue
        (Event from p name) = e
        m = case Map.lookup name dict of
              Nothing -> error $ "Should not happen" ++ show name
              Just m -> m
        (m',events) = process name m (from, p)
        dict' = Map.insert name m' dict
        (dl,dh) = countEventPulse e
        newQueue = pushList queue' $ fromMaybe [] events
   in
    put (dict',newQueue,(low+dl,high+dh),buttonPressed) >>
    handlePulse


realName :: String -> String
realName "broadcaster" = "broadcaster"
realName "output" = "output"
realName ('%':name) = name
realName ('&':name) = name
realName _ = error "unexpected value found"


extractBasicModule :: [String] -> Map.Map String Module
extractBasicModule contents = foldl' go Map.empty contents
   where go dict str = let from = head $ words str
                           names = map (filter (/=',')) $ tail $ tail $ words str
                       in
                         case from of
                           "broadcaster" -> Map.insert from (Broadcaster names) dict
                           ('%':name) -> Map.insert name (Flip_Flop Off names) dict
                           ('&':name) -> Map.insert name (Conjunction (Map.empty) names) dict
prepareMachine :: Map.Map String Module ->  [String] -> Map.Map String Module
prepareMachine dict contents = foldl' go dict contents
  where go dict str = let from = realName $ head $ words str
                          names = map (filter (/=',')) $ tail $ tail $ words str
                      in
                        foldl' (assignInput from) dict names
        assignInput from dict name = case Map.lookup name dict of
                                       Nothing ->  Map.insert name Output dict 
                                       Just m -> case m of
                                         Broadcaster _ -> dict
                                         Flip_Flop _ _ -> dict
                                         Conjunction inputs outputs -> let inputs' = Map.insert from Low inputs in
                                                                         Map.insert name (Conjunction inputs' outputs) dict
                                    
                         
                          
  

main :: IO ()
main = readFile "input.txt" <&> lines <&> solvePart1 <&> show >>= putStrLn
-- solvePart1 :: [String] -> [Module]
solvePart1 contents = let basicModule = extractBasicModule contents 
                          modules = Map.insert "button" Button $ Map.insert "output" Output $ prepareMachine basicModule contents
                          pressButton = singleton (Event "button" Low "broadcaster")
                          (_,_,(low,high),buttonPressed) = execState handlePulse (modules,pressButton,(0,0),1)
                          cycles = 1000 `div` buttonPressed
                      in
                        product [low, cycles,high,cycles]
