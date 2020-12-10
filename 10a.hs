import Data.List
import qualified Data.Set as S
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ words $ input

pairs :: [Int] -> [(Int, Int)]
pairs xs = zip xs (tail xs)

addSocketAndLast :: [Int] -> [Int]
addSocketAndLast xs = (0 : xs ++ [last xs + 3])

diff :: [(Int, Int)] -> [Int]
diff = foldl' (\acc x -> acc ++ [snd x - fst x]) []

filterInt :: Int -> [Int] -> [Int]
filterInt x = filter (x ==)

getDiffs :: [Int] -> Int
getDiffs xs = (length $ filterInt 1 xs) * (length $ filterInt 3 xs)

fn xs = getDiffs $ diff $ pairs $ addSocketAndLast $ sort $ map (read :: String -> Int) xs
