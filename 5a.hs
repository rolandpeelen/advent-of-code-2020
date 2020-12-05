{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Text.ParserCombinators.Parsec
import Prelude

magicRowPartitionLength = 7

data Rounding = Up | Down

type BinPartLst = String

type LowerChar = Char

type UpperChar = Char

type BinPartsInstr = (LowerChar, UpperChar, Integer, Integer)

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

divRnd :: Rounding -> Integer -> Integer
divRnd Up x = fst $ x `divMod` 2
divRnd Down x = (+ 1) $ fst $ x `divMod` 2

walkBinaryPartition :: BinPartsInstr -> BinPartLst -> Integer
walkBinaryPartition (lowerC, _, left, right) [char] = if char == lowerC then left else right
walkBinaryPartition (lowerC, upperC, left, right) (head : xs) = case (head == lowerC) of
  True -> walkBinaryPartition (lowerC, upperC, left, right - divRnd Up (right - left)) xs
  False -> walkBinaryPartition (lowerC, upperC, right - divRnd Down (right - left), right) xs

genSeatId :: (BinPartLst, BinPartLst) -> Integer
genSeatId (row, seat) = ((walkBinaryPartition ('F', 'B', 0, 127) row) * 8) + (walkBinaryPartition ('L', 'R', 0, 7) seat)

maxAcc :: Integer -> Integer -> Integer
maxAcc acc x = if x > acc then x else acc

maxAccList :: [Integer] -> Integer
maxAccList xs = foldl maxAcc 0 xs

fn :: [String] -> Integer
fn xs = maxAccList $ map genSeatId $ map (splitAt magicRowPartitionLength) xs
