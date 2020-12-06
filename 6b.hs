import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ input

groupAnswer :: [String] -> [[String]]
groupAnswer = map (filter ((/=) "")) . groupBy (\_ y -> y /= "")

mergeAnswer :: [String] -> (Int, String)
mergeAnswer = \ys -> (length ys, unwords ys)

countOccurances :: Eq a => [a] -> a -> Int
countOccurances xs y = length $ filter (y==) xs

-- This does, right to left:
-- $ create a list of unique items from the answers
-- $ map the unique items, replace each of them with the count of them in the original list
-- $ filter where the count is equal to the amount of group members (ie. 3 people answered, filter the items where all 3 did for that specific answer)
-- check length 
checkAnswer :: (Int, String) -> Int
checkAnswer (count, answers) = length $ filter (count ==) $ map (countOccurances answers) $ nub answers

fn :: String -> Int
fn xs = sum $ map (checkAnswer . mergeAnswer) $ groupAnswer $ lines xs
