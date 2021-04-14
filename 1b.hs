import Data.List
import Prelude

main :: IO ()
main = do
  input <- getContents
  (putStr . show . fn . map (read . filterChars "+") . lines) input

filterChars :: String -> String -> String
filterChars xs = filter (not . (`elem` xs))

findNo :: [Int] -> Int -> Int -> [Int] -> Int
findNo seen count idx input = case nextSeen of
  Just no -> no
  Nothing -> findNo (current : seen) current (idx + 1) input
  where
    current = count + (input !! idx)
    nextSeen = find (== current) seen

fn :: [Int] -> Int
fn xs = findNo [] 0 0 (cycle xs)
