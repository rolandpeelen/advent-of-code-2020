import Data.List
import qualified Data.Set as S
import Prelude

main :: IO ()
main = do
  input <- getContents
  (putStr . show . fn . map (read . filterChars "+") . lines) input

filterChars :: String -> String -> String
filterChars xs = filter (not . (`elem` xs))

findNo :: S.Set Int -> Int -> [Int] -> Int
findNo seen count (current : rest) =
  if seenCount'
    then count'
    else findNo seen' count' rest
  where
    count' = count + current
    seen' = S.insert count' seen
    seenCount' = S.member count' seen

fn :: [Int] -> Int
fn xs = findNo S.empty 0 (cycle xs)
