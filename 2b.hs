{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Text.ParserCombinators.Parsec
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | (i > -1) && (length xs > i) = Just (xs !! i)
  | otherwise = Nothing

charMatchAt :: Int -> Char -> [Char] -> Bool
charMatchAt x char str = case safeIndex str x of
  Just y -> y == char
  _ -> False

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
passwordChecker (min, max, char, pass) = case (charMatchAt (min - 1) char pass, charMatchAt (max - 1) char pass) of
  (True, True) -> 0
  (False, False) -> 0
  _ -> 1

folder :: Int -> String -> Int
folder acc xs = case parse passwordParser "Some Error" xs of
  Left _ -> acc
  Right passwordData -> acc + (passwordChecker passwordData)

fn :: [String] -> Int
fn xs = foldl folder 0 xs
