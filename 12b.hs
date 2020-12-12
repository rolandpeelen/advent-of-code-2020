{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Text.Parsec hiding (count)
import Prelude

type Coordinate = (Int, Int)

type Waypoint = (Int, Int)

type Position = (Coordinate, Waypoint)

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
plusX z (sp, (x, y)) = (sp, (x + z, y))

plusY :: Int -> Position -> Position
plusY z (sp, (x, y)) = (sp, (x, y + z))

rotate :: Instruction -> Position -> Position
rotate (R 90) (sp, (x, y)) = (sp, (negate y, x))
rotate (L 90) (sp, (x, y)) = (sp, (y, negate x))
rotate (L 180) (sp, (x, y)) = (sp, (negate x, negate y))
rotate (R 270) p = rotate (L 90) p
rotate (L 270) p = rotate (R 90) p
rotate (R 180) p = rotate (L 180) p

forward :: Int -> Position -> Position
forward z ((x, y), (a, b)) = ((x + (a * z), y + (b * z)), (a, b))

getNextCoordinate :: Position -> Instruction -> Position
getNextCoordinate p (N z) = plusY (- z) p
getNextCoordinate p (S z) = plusY z p
getNextCoordinate p (W z) = plusX (- z) p
getNextCoordinate p (E z) = plusX z p
getNextCoordinate p (F z) = forward z p
getNextCoordinate p (R z) = rotate (R z) p
getNextCoordinate p (L z) = rotate (L z) p

startPosition = ((0, 0), (10, -1))

getMannhattanDistance :: Coordinate -> Int
getMannhattanDistance (x, y) = abs $ x + y

fn xs = getMannhattanDistance . fst $ foldl' getNextCoordinate startPosition $ rights $ map (runParser instructionP () "") xs
