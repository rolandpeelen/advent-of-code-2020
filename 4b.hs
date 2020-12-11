{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn input

data Validity = Valid | InValid | Skipped deriving (Eq)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | (i > -1) && (length xs > i) = Just (xs !! i)
  | otherwise = Nothing

charMatchAt :: Int -> Char -> [Char] -> Bool
charMatchAt x char str = case safeIndex str x of
  Just y -> y == char
  _ -> False

boolToValidity :: Bool -> Validity
boolToValidity True = Valid
boolToValidity False = InValid

isBetween :: Int -> Int -> Int -> Bool
isBetween x y z = z >= x && z <= y

validateHeight :: Maybe String -> Maybe String -> Validity
validateHeight (Just "cm") (Just height) = boolToValidity $ isBetween 150 193 (read height)
validateHeight (Just "in") (Just height) = boolToValidity $ isBetween 59 76 (read height)
validateHeight _ _ = boolToValidity False

lengthIs :: Int -> [a] -> Bool
lengthIs x xs = length xs == x

isEyeColor :: String -> Bool
isEyeColor x = case find (x ==) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] of
  Just _ -> True
  Nothing -> False

birthyearP = parserGen "byr" (\x -> boolToValidity $ lengthIs 4 x && isBetween 1920 2002 (read x))

issueYearP = parserGen "iyr" (\x -> boolToValidity $ lengthIs 4 x && isBetween 2010 2020 (read x))

expirationYearP = parserGen "eyr" (\x -> boolToValidity $ lengthIs 4 x && isBetween 2020 2030 (read x))

passportIdP = parserGen "pid" boolToValidity . lengthIs 9

eyeColorP = parserGen "ecl" (\x -> boolToValidity $ lengthIs 3 x && isEyeColor x)

countryIdP = parserGen "cid" const Skipped

heightP :: GenParser Char Int Validity
heightP = do
  try $ string "hgt"
  char ':'
  height <- optionMaybe $ many1 digit
  heightUnit <- optionMaybe $ many1 letter
  choice [space, newline]
  return (validateHeight heightUnit height)

hairColorP :: GenParser Char Int Validity
hairColorP = do
  try $ string "hcl"
  char ':'
  color <- many1 $ choice [char '#', alphaNum]
  choice [space, newline]
  return $ boolToValidity $ lengthIs 7 color && charMatchAt 0 '#' color

parserGen :: String -> (String -> Validity) -> GenParser Char Int Validity
parserGen x validator = do
  try $ string x
  char ':'
  str <- many1 $ choice [char '#', alphaNum]
  choice [space, newline]
  return $ validator str

singlePassportParser :: GenParser Char Int ()
singlePassportParser = do
  valids <-
    many1 $
      choice
        [ birthyearP,
          issueYearP,
          expirationYearP,
          heightP,
          hairColorP,
          eyeColorP,
          passportIdP,
          countryIdP
        ]
  state <- getState
  setState (if length filter (== Valid) valids >= 7 then state + 1 else state)
  return ()

passportParser :: GenParser Char Int Int
passportParser = do
  many1 $ choice [singlePassportParser, skipMany1 newline]
  eof
  getState

fn :: String -> String
fn x = case runParser passportParser 0 "Err" x of
  Left err -> show err
  Right x -> show x
