{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.List
import Data.List.Index
import qualified Data.Map as M
import Text.Parsec hiding (count)
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn input

data Action = Nop Int | Acc Int | Jump Int | Done deriving (Show)

-- START - Parsing
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
  t <$> intP

instructionP :: Parsec String () Action
instructionP =
  actionPG "nop" const Nop
    <|> actionPG "acc" Acc
    <|> actionPG "jmp" Jump

setDone :: Maybe Action -> Maybe Action
setDone _ = Just Done

swapNopJump :: Maybe Action -> Maybe Action
swapNopJump (Just (Nop x)) = Just (Jump x)
swapNopJump (Just (Jump x)) = Just (Nop x)
swapNopJump _ = Just Done

incr :: Int -> Int
incr x = x + 1

-- END - Parsing

type Swap = Int

type NextIndex = Int

type Count = Int

type Step = Int

type Acc = (Count, Step)

type InstructionConfig = (Acc, Swap, NextIndex)

data Exit = Success Int | Loop Int deriving (Show)

-- Take an instruction config ((initialCount, initialStep), swapIdx, nextIdx)
-- Then take a map of instructions
-- Run them, swaping the Nop to Jump / Jump to Nop at the swapIdx
runInstructions :: InstructionConfig -> M.Map Int Action -> Exit
runInstructions ((count, step), swapIdx, nextIdx) map = case (step == swapIdx, M.lookup nextIdx map) of
  (_, Nothing) -> Success count
  (_, Just Done) -> Loop count
  (_, Just (Acc x)) -> runInstructions ((count + x, incr step), swapIdx, incr nextIdx) (M.alter setDone nextIdx map)
  -- Execute as normal
  (False, Just (Nop _)) -> runInstructions ((count, incr step), swapIdx, incr nextIdx) (M.alter setDone nextIdx map)
  (False, Just (Jump x)) -> runInstructions ((count, incr step), swapIdx, nextIdx + x) (M.alter setDone nextIdx map)
  -- Swap instruction and try again
  (True, _) -> runInstructions ((count, incr step), swapIdx, nextIdx) (M.alter swapNopJump nextIdx map)

-- Try running every single version of the program swapping an element at a certain index
-- until we find one that returns successfully
runAllInstructions :: Int -> M.Map Int Action -> Exit
runAllInstructions x ms = case (x == 198, runInstructions ((0, 0), x, 0) ms) of
  (_, Success acc) -> Success acc
  (False, Loop _) -> runAllInstructions (x + 1) ms
  (True, Loop acc) -> Loop acc

fn xs = runAllInstructions 0 $ M.fromList $ indexed $ rights $ map (runParser instructionP () "") $ lines xs
