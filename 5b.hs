{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.List
import Prelude

magicRowPartitionLength = 7

data Rounding = Up | Down

type BinPartLst = String

type Row = Int

type Seat = Int

type Place = (Row, Seat)

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

safeRndUp :: (Int, Int) -> Int
safeRndUp (x, 0) = x
safeRndUp (x, _) = x + 1

divRnd :: Rounding -> Int -> Int
divRnd Up x = safeRndUp $ x `divMod` 2
divRnd Down x = fst $ x `divMod` 2

walkBinaryPartitionLs :: (Char, Char) -> [Int] -> [Char] -> Int
walkBinaryPartitionLs (lower, _) [x, y] [char] = if char == lower then x else y
walkBinaryPartitionLs (lower, upper) xs (head : ys) = case (head == lower) of
  True -> walkBinaryPartitionLs (lower, upper) (take (length xs `div` 2) xs) ys
  False -> walkBinaryPartitionLs (lower, upper) (drop (length xs `div` 2) xs) ys

genSeatId :: (BinPartLst, BinPartLst) -> Int
genSeatId (row, seat) = (((walkBinaryPartitionLs ('F', 'B') [0 .. 127] row) * 8) + (walkBinaryPartitionLs ('L', 'R') [0 .. 7] seat))

findMissing:: [Int] -> Int
findMissing xs = sum [head xs .. head xs+length xs] - sum xs

--
--fn :: [String] -> Maybe Int
fn xs = findMissing $ sort $ map genSeatId $ map (splitAt magicRowPartitionLength) xs
