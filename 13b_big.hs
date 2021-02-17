{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Bifunctor as BF
import Data.List
import Data.List.Split
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ toTuple $ lines input

indexedI :: [a] -> [(Integer, a)]
indexedI = go 0 
  where
    go i (a:as) = (i, a) : go (i + 1) as
    go _ _      = []

toTuple :: [String] -> [(Integer, Integer)]
toTuple [_, y] = map (BF.second read) $ filter ((/= "x") . snd) $ indexedI $ splitOn "," y

type Timestamp = Integer

departsExactly :: Timestamp -> (Integer, Integer) -> Bool
departsExactly time (idx, val) = val - (time `mod` val) == idx || (time `mod` val == 0 && idx == 0)

checkAtTime t = all ((== True) . departsExactly t)

check :: (Integer, Integer) -> [(Integer, Integer)] -> (Integer, Integer)
check (c, t) xs = if checkAtTime t xs then (c, t) else check (c + 1, t - 1) xs

getMax :: [(Integer, Integer)] -> Integer
getMax = product . map snd

--fn :: [ String ] -> Maybe Integer
fn xs = check (0, getMax xs) xs
