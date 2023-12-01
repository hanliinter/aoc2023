{-# LANGUAGE BlockArguments #-}
module Main where

import Data.Char
import Data.List
import Data.Maybe

--part1 :: [String] -> Int

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString niddle hay = findIndex (isPrefixOf niddle) (tails hay)

findStringEx :: (Eq a) => [a] -> [a] -> Maybe (Int, [a])
findStringEx niddle hay = findString niddle hay >>= \i ->Just (i,niddle)

                                                    
                                                    

replaceAllStringsEx :: [String] ->String-> String
replaceAllStringsEx niddles hay = let findResult = sort $ filter (/= Nothing) $ map (\x -> findStringEx x hay) niddles in
                                    if length findResult == 0
                                    then hay
                                    else
                                      let Just (i, niddle) = head findResult
                                          (pre,rest) = splitAt i hay
                                          suffix = drop (length niddle) rest
                                          replaced = name2int niddle
                                          hay' = pre ++ replaced ++ suffix
                                      in
                                        replaceAllStringsEx niddles hay'
                                        
                                      



nameTable :: [(String,String)]
nameTable = [("one", "o1ne"),
         ("two", "t2wo"),
         ("three","t3hree"),
         ("four", "f4our"),
         ("five", "f5ive"),
         ("six", "s6ix"),
         ("seven","s7even"),
         ("eight", "e8ight"),
          ("nine", "n9ine")]


names :: [String]
names = ["one",
         "two",
         "three",
         "four",
         "five",
         "six",
         "seven",
          "eight",
          "nine"]


name2int :: String -> String
name2int target = case lookup target nameTable of
                    Just c -> c
                    Nothing -> error "should never happen"

solve_part2 :: FilePath -> IO ()
solve_part2 file = do
  input <- readFile file
  let contents = lines input
      result  = sum (map go contents)
  putStrLn $ show $ result 
    where go str = let str' = replaceAllStringsEx names str
                       xs =  map (read . singleton) (filter isDigit str')
                       a = head xs
                       b = head $ reverse xs
                   in
                     a * 10 + b --


-- show_lines :: FilePath -> IO ()
-- show_lines file = do
--   input <- readFile file
--   let contents = lines input
--   mapM_ (\x -> do
--            putStrLn "-------------"
--            putStrLn x
--            putStrLn $ (replaceAllStringsEx names x)) contents


solve_part1 :: FilePath -> IO ()
solve_part1 file = do
  input <- readFile file
  let contents = lines input
      result  = sum (map go contents )
  putStrLn $ show $ result 
    where go str = let xs =  map (read . singleton) (filter isDigit str)
                       a = head xs
                       b = head $ reverse xs
                   in
                     a * 10 + b


  

main :: IO ()
main = solve_part1 "input.txt"
