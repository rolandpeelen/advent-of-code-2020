{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec
import Data.Either
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input


--7-10 q: kqhcqqqqqqqqn
passwordParser :: GenParser Char st (Int, Int, Char, String)
passwordParser = do
  n1 <- many1 digit
  char '-'
  n2 <- many1 digit
  spaces
  c1 <- letter
  char ':'
  spaces
  pass <- many1 letter 
  return (read n1, read n2, c1, pass)

passwordChecker :: (Int, Int, Char, String) -> Int
passwordChecker (min, max, char, pass)
  | min <= occ && occ <= max = 1
  | otherwise = 0
  where
    occ = foldl (\y x -> if x == char then y + 1 else y) 0 pass

folder :: Int -> String -> Int
folder acc xs = case parse passwordParser "Some Error" xs of
    Left _ -> acc 
    Right passwordData -> acc + (passwordChecker passwordData)

fn :: [String] -> Int
fn xs = foldl folder 0 xs
