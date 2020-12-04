{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Text.ParserCombinators.Parsec
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ input


birthyearP = parserGen "byr" True

issueYearP = parserGen "iyr" True

expirationYearP = parserGen "eyr" True

heightP = parserGen "hgt" True

hairColorP = parserGen "hcl" True

eyeColorP = parserGen "ecl" True

passportIdP = parserGen "pid" True

countryIdP = parserGen "cid" False

parserGen :: String -> Bool -> GenParser Char Int Int
parserGen x required = do
  try $ string x
  char ':'
  optional $ char '#'
  many1 alphaNum
  choice [space, newline]
  if required then return 1 else return 0

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
  setState (if (sum valids >= 7) then state + 1 else state)
  return ()

passportParser :: GenParser Char Int Int
passportParser = do
  many1 $ choice [singlePassportParser, skipMany1 newline]
  eof
  state <- getState
  return state

fn :: String -> String
fn x = case runParser passportParser 0 "Err" x of
  Left err -> show err
  Right x -> show $ x
