{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Text.Parsec hiding (count)
import Prelude

type Coordinate = (Int, Int)

type Heading = Int

type Position = (Coordinate, Heading)

data Instruction
  = N Int
  | W Int
  | S Int
  | E Int
  | L Int
  | R Int
  | F Int
  deriving (Show, Eq)

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

dataIntP :: String -> (Int -> a) -> Parsec String () a
dataIntP x t = do
  try $ string x
  t . read <$> many1 digit

instructionP :: Parsec String () Instruction
instructionP =
  dataIntP "N" N
    <|> dataIntP "W" W
    <|> dataIntP "S" S
    <|> dataIntP "E" E
    <|> dataIntP "L" L
    <|> dataIntP "R" R
    <|> dataIntP "F" F

plusX :: Int -> Position -> Position
plusX z ((x, y), heading) = ((x + z, y), heading)

plusY :: Int -> Position -> Position
plusY z ((x, y), heading) = ((x, y + z), heading)

plusH :: Int -> Position -> Position
plusH z ((x, y), heading) = ((x, y), (heading + z) `mod` 360)

forward :: Int -> Position -> Position
forward z (c, 0) = plusY (- z) (c, 0)
forward z (c, 180) = plusY z (c, 180)
forward z (c, 90) = plusX z (c, 90)
forward z (c, 270) = plusX (- z) (c, 270)

getNextCoordinate :: Position -> Instruction -> Position
getNextCoordinate p (N z) = plusY (- z) p
getNextCoordinate p (S z) = plusY z p
getNextCoordinate p (W z) = plusX (- z) p
getNextCoordinate p (E z) = plusX z p
getNextCoordinate p (L z) = plusH (- z) p
getNextCoordinate p (R z) = plusH z p
getNextCoordinate p (F z) = forward z p

startPosition = ((0, 0), 90)

getMannhattanDistance :: Coordinate -> Int
getMannhattanDistance (x, y) = abs $ x + y

fn xs = getMannhattanDistance . fst $ foldl' getNextCoordinate startPosition $ rights $ map (runParser instructionP () "") xs
