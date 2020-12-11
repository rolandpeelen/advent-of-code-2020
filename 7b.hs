{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Text.Parsec hiding (count)
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

type BagName = String

type BagCount = Int

type InnerBag = (Int, String)

type OuterBag = (BagName, [InnerBag])

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
addToCount = foldl (\acc x -> fst x + acc)

-- Given a list of Outerbags, filter them by name
filterBag :: BagName -> [OuterBag] -> [OuterBag]
filterBag bagName = filter (\y -> fst y == bagName)

-- Multiple the amount of the bags that can be inside one other bag
-- by the parent bag. Multiplying because if we have 10 bags, that all can have
-- 5 other bags in which our bag could go, then we would have 10 * 5 = 50 bags
-- in which to put ours.
-- ie. 10 [(5, "b"), (3, "c")] where 10 is the amount of bags in the parent
-- to [(50, "b"), (30, "c")]
multiplyCountBy :: BagCount -> [InnerBag] -> [InnerBag]
multiplyCountBy count = map (\y -> (fst y * count, snd y))

-- This takes a list of outer bags and an innerbag,
-- [("a", [(4, "b"),(12, "c")]), ("e", [(2, "f"), (4, "a")])] (12, "e")
-- Filters them (filtered by the name of some other bag)
-- [("e", [(2, "f"), (4, "a")])]
-- Takes the list of bags that's inside of that bag
-- [[(2, "f"), (4, "a")]]
-- Concats that
-- [[(2, "f"), (4, "a")]]
-- Multiplies every item by the initial occurances
-- [[(2, "f"), (4, "a")]] 12
-- [[(24, "f"), (48, "a")]] 12
countBags :: [OuterBag] -> InnerBag -> [InnerBag]
countBags xs (occurances, bagName) = multiplyCountBy occurances $ concatMap snd $ filterBag bagName xs

-- Given a list of Inners, map them to the structure above
-- [(12, "e"), (3, "a")]
-- [[(12, "b"), (36, "c")], [(24, "f"), (48, "a")]]
findChildren :: [InnerBag] -> [OuterBag] -> [InnerBag]
findChildren xs ys = concatMap (countBags ys) xs

-- A recursive wrapper to the one above, that takes the output, and for every
-- iteration, add the amount of bags to the count
-- [[(12, "b"), (36, "c")], [(24, "f"), (48, "a")]]
-- count -> 12 + 36 + 24 + 48
findChildrenRec :: Int -> [InnerBag] -> [OuterBag] -> Int
findChildrenRec acc xs ys = case findChildren xs ys of
  [] -> acc
  zs -> findChildrenRec (addToCount acc zs) zs ys

fn xs = findChildrenRec 0 [(1, "shiny gold")] $ rights $ map (runParser bagParser () "") xs
