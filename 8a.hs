{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Data.List.Index
import qualified Data.Map as M
import Text.Parsec hiding (count)
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ input

data Action = Nop | Acc (Int) | Jump (Int) | Done deriving (Show)

intP :: Parsec String () Int
intP = read <$> (plus <|> minus <|> number)
  where
    plus = char '+' *> number
    minus = (:) <$> char '-' <*> number
    number = many1 digit

actionPG :: String -> (Int -> Action) -> Parsec String () Action
actionPG x t = do
  try $ string x
  space
  action <- t <$> intP
  return action

instructionP :: Parsec String () Action
instructionP =
  actionPG "nop" (\_ -> Nop)
    <|> actionPG "acc" (\x -> Acc (x))
    <|> actionPG "jmp" (\x -> Jump (x))

setDone :: Maybe Action -> Maybe Action
setDone _ = Just Done

runInstructions :: Int -> Int -> M.Map Int Action -> Int
runInstructions acc idx map = case (M.lookup idx map) of 
  Nothing -> acc
  Just Done -> acc
  Just (Acc x) -> runInstructions (acc + x) (idx+1) (M.alter setDone idx map)
  Just (Jump x) -> runInstructions acc (idx+x) (M.alter setDone idx map)
  Just Nop -> runInstructions acc (idx+1) (M.alter setDone idx map)

fn xs = runInstructions 0 0 $ M.fromList $ indexed $ rights $ map (runParser instructionP () "") $ lines xs
