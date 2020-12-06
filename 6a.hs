{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Data.List

magicRowPartitionLength = 7

main = do
  input <- getContents
  putStr $ show $ fn $ input

concatIfNextEmpty :: [String] -> [String] -> [String]
concatIfNextEmpty acc [] = acc
concatIfNextEmpty acc [a, b] 
  | a == "" && b == "" = acc
  | a /= "" && b == "" = (acc++[a])
  | otherwise = acc++[a++b]
concatIfNextEmpty acc (a:b:xs)
  | a == "" && b == "" = concatIfNextEmpty acc xs
  | a /= "" && b == "" = concatIfNextEmpty ( acc++[a] ) xs
  | otherwise = concatIfNextEmpty acc ((a++b):xs)



--fn :: [String] -> [ C ]
fn xs = sum $ map length $ map nub $ concatIfNextEmpty [] $ lines xs
