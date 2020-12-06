{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Data.List

magicRowPartitionLength = 7

main = do
  input <- getContents
  putStr $ show $ fn $ input

groupAnswers :: [String] -> [[ String ]]
groupAnswers xs = map (filter ((/=) "")) $ groupBy(\_ y -> y /= "") xs

mergeAnswers :: [[String]] -> [(Int, String)]
mergeAnswers xs = map (\ys -> (length ys, unwords ys)) xs

checkAnswer :: (Int, String) -> Int
checkAnswer (count, answers) = length $ filter (count==) $ map (\x -> (length $ filter (x==) answers)) $ nub answers 

fn :: String -> Int 
fn xs = sum $ map checkAnswer $ mergeAnswers $ groupAnswers $ lines xs
