import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ words input

pairs :: [Int] -> [(Int, Int)]
pairs xs = zip xs (tail xs)

addSocketAndLast :: [Int] -> [Int]
addSocketAndLast xs = 0 : xs ++ [last xs + 3]

diff :: [(Int, Int)] -> [Int]
diff = foldl' (\acc x -> acc ++ [snd x - fst x]) []

calcFullPermutationSet :: [Int] -> [Int]
calcFullPermutationSet x = map calcPermutations $ filter (\x -> head x == 1) $ group x

calcPermutations :: [Int] -> Int
calcPermutations x
  | l == 4 = 7
  | otherwise = 2 ^ (l -1)
  where
    l = length x

fn xs = product $ calcFullPermutationSet $ diff $ pairs $ addSocketAndLast $ sort $ map (read :: String -> Int) xs
