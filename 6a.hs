import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn input

groupAnswers :: [String] -> [[String]]
groupAnswers = map (filter ("" /=)) . groupBy (\_ y -> y /= "")

fn :: String -> Int
fn xs = sum $ map (length . nub . unwords) $ groupAnswers $ lines xs
