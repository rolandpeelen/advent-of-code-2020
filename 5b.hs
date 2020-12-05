{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.List
import Prelude

magicRowPartitionLength = 7

data Rounding = Up | Down

type Row = Int

type Seat = Int

type Place = (Row, Seat)

data Half = LeftH | RightH

type PartitionSteps = [Char]

type PartitionChars = (Char, Char)

type PartitionInts = (Int, Int)

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

getNextHalf :: Half -> PartitionInts -> (Int, Int)
getNextHalf LeftH (left, right) = (left, right - (right - left + 1) `div` 2)
getNextHalf RightH (left, right) = (left + (right - left + 1) `div` 2, right)

walkPartition :: PartitionChars -> PartitionInts -> String -> Int
walkPartition pChars (left, right) [char] = if char == fst pChars then left else right
walkPartition pChars pInts (head : xs)
  | head == fst pChars = walkPartition pChars (getNextHalf LeftH pInts) xs
  | head == snd pChars = walkPartition pChars (getNextHalf RightH pInts) xs
  where

genRow :: PartitionSteps -> Row
genRow row = walkPartition ('F', 'B') (0, 127) row

genSeat :: PartitionSteps -> Seat
genSeat seat = walkPartition ('L', 'R') (0, 7) seat

genPlaceId :: (String, String) -> Int
genPlaceId (row, seat) = (genRow row * 8) + genSeat seat

findAllMissing :: [Int] -> [Int] -> [Int]
findAllMissing acc [] = acc
findAllMissing acc [a, b] 
  | succ a == b = acc 
  | otherwise = ([succ a..pred b]++acc)
findAllMissing acc (a:b:xs)
  | succ a == b = findAllMissing acc (b:xs)
  | otherwise = findAllMissing ([succ a..pred b]++acc) (b:xs)


fn :: [String] -> [Int]
fn xs = findAllMissing [] $ sort $ map genPlaceId $ map (splitAt magicRowPartitionLength) xs
