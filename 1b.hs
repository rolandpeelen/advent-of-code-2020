import Data.List
import Prelude

main :: IO ()
main = do
  input <- getContents
  (putStr . show . fn . map (read . filterChars "+") . lines) input

filterChars :: String -> String -> String
filterChars xs = filter (not . (`elem` xs))

findNo :: [Int] -> Int -> [Int] -> Int
findNo seen count (current : rest) = case seenCount' of
  Just no -> no
  Nothing -> findNo seen' count' rest
  where
    seenCount' = find (== count') seen
    count' = count + current
    seen' = count' : seen

fn :: [Int] -> Int
fn xs = findNo [] 0 (cycle xs)
