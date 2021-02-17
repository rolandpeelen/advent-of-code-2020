{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.List.Split
import Prelude

type MyTime = Int

type Busses = [Int]

type Schedule = (MyTime, Busses)

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

toTuple :: [String] -> Schedule
toTuple [x, y] = (read x, map read $ filter (/= "x") $ splitOn "," y)

addMod :: Schedule -> ([Int], [Int])
addMod (t, xs) = (xs, map (\x -> x - t `mod` x) xs)

calcId (xs, ys) = (* min) . (!!) xs <$> elemIndex min ys
  where
    min = minimum ys

fn :: [String] -> Maybe Int
fn x = calcId $ addMod $ toTuple x
