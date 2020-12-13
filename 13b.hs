{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Bifunctor as BF
import Data.List
import Data.List.Split
import Data.Tuple
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ splitAndIndexed $ lines input

indexedI :: [a] -> [(Integer, a)]
indexedI = go 0
  where
    go i (a : as) = (i, a) : go (i + 1) as
    go _ _ = []

splitAndIndexed :: [String] -> [(Integer, Integer)]
splitAndIndexed [_, y] = map (BF.second read) $ filter ((/= "x") . snd) $ indexedI $ splitOn "," y

fn :: [(Integer, Integer)] -> Integer
fn = fst . foldl nextPair (0, 1)
  where
    nextPair (lastMatch, nextSearch) (busIdx, busNo) = (nextMatch, nextSearch * busNo)
      where
        nextMatch = until (\n -> (n + busIdx) `mod` busNo == 0) (+ nextSearch) lastMatch
