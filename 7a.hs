{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Data.Maybe
import Text.Parsec hiding (count)
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

type Inner = String

type Outer = String

type Bag = (Outer, [Inner])

type Inversed = (Inner, Outer)

--mirrored gold bags contain 3 light teal bags.
innerBag :: Parsec String () String
innerBag = do
  try space
  amount <- digit
  try space
  innerBag <- manyTill anyChar $ try $ string " bag"
  optional $ char 's'
  choice [char ',', char '.']
  return innerBag

outerBag :: Parsec String () String
outerBag = do
  outerBag <- manyTill anyChar $ try $ string " bags" -- Note leading space
  try $ string " contain"
  return outerBag

bagParser :: Parsec String () Bag
bagParser = do
  outerBag <- outerBag
  innerBags <- many1 innerBag
  eof
  return (outerBag, innerBags)

-- Make (Outer, [Inners]) to [(Inner, Outer)]
inverseBag :: Bag -> [Inversed]
inverseBag (x, xs) = map (,x) xs

-- Make [(Outer, [Inners])] to [[(Inner, Outer)]]
inverseBags :: [Bag] -> [Inversed]
inverseBags = concatMap inverseBag

-- From a list of strings ('inner'), find a list of strings ('outer')
findParents :: [String] -> [Inversed] -> [String]
findParents xs ys = map snd $ concatMap (\x -> filter ((x ==) . fst) ys) xs

-- From a list of strings ('inner'), find a list of strings ('outer')
-- If there are 'outer' strings, do it again
findParentsRec :: [String] -> [String] -> [Inversed] -> [String]
findParentsRec acc xs ys = case findParents xs ys of
  [] -> acc
  zs -> findParentsRec (zs ++ acc) zs ys

fn xs = length $ nub $ findParentsRec [] ["shiny gold"] $ inverseBags $ rights $ map (runParser bagParser () "Err") xs
