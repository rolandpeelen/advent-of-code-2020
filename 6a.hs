import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ input

groupAnswers :: [String] -> [[String]]
groupAnswers xs = map (filter ((/=) "")) $ groupBy (\_ y -> y /= "") xs

fn :: String -> Int
fn xs = sum $ map (length . nub . unwords) $ groupAnswers $ lines xs
