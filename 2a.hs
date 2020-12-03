{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec
import Data.Either
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

passwordParser :: GenParser Char st (Int, Int, Char, String)
passwordParser = do
  min <- many1 digit
  char '-'
  max <- many1 digit
  spaces
  character <- letter
  char ':'
  spaces
  password <- many1 letter
  return (read min, read max, character, password)

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
