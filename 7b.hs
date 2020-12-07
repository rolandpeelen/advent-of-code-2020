{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Text.Parsec hiding (count)
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines $ input

type BagName = String

type BagCount = Int

type InnerBag = (Int, String)

type OuterBag = (BagName, [(InnerBag)])

innerBag :: Parsec String () (Int, String)
innerBag = do
  try space
  amount <- many1 digit
  try space
  innerBag <- manyTill anyChar $ try $ string " bag"
  optional $ char 's'
  choice [char ',', char '.']
  return (read amount, innerBag)

outerBag :: Parsec String () String
outerBag = do
  outerBag <- manyTill anyChar $ try $ string " bags" -- Note leading space
  try $ string " contain"
  return outerBag

bagParser :: Parsec String () OuterBag
bagParser = do
  outerBag <- outerBag
  innerBags <- many1 innerBag
  eof
  return (outerBag, innerBags)

addToCount :: BagCount -> [InnerBag] -> Int
addToCount acc xs = foldl (\acc x -> (fst x) + acc) acc xs

filterBag :: BagName -> [OuterBag] -> [OuterBag]
filterBag bagName = filter (\y -> (fst y) == bagName)

multiplyCountBy :: BagCount -> [InnerBag] -> [InnerBag]
multiplyCountBy count = map (\y -> (((fst y) * count), snd y))

countBags :: [OuterBag] -> InnerBag -> [InnerBag]
countBags xs (occurances, bagName) = multiplyCountBy occurances $ concat $ map snd $ filterBag bagName xs

findChildren :: [InnerBag] -> [OuterBag] -> [InnerBag]
findChildren xs ys = concat $ map (countBags ys) xs

findChildrenRec :: Int -> [InnerBag] -> [OuterBag] -> Int
findChildrenRec acc xs ys = case (findChildren xs ys) of
  [] -> acc
  zs -> findChildrenRec (addToCount acc zs) zs ys

fn xs = findChildrenRec 0 [(1, "shiny gold")] $ rights $ map (runParser bagParser () "") xs
