{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Prelude

magicRowPartitionLength = 7

main = do
  input <- getContents
  putStr $ show $ fn $ input

groupAnswer :: [String] -> [[String]]
groupAnswer xs = map (filter ((/=) "")) $ groupBy (\_ y -> y /= "") xs

mergeAnswer :: [String] -> (Int, String)
mergeAnswer x = (\ys -> (length ys, unwords ys)) x

checkAnswer :: (Int, String) -> Int
checkAnswer (count, answers) = length $ filter (count ==) $ map (\x -> (length $ filter (x ==) answers)) $ nub answers

fn :: String -> Int
fn xs = sum $ map (checkAnswer . mergeAnswer) $ groupAnswer $ lines xs
