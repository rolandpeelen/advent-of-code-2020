import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ find2020 (-1) (sort $ map read $ lines input)

find2020 :: Int -> [Int] -> Int
find2020 _ [] = -1
find2020 (-1) (x : xs) = find2020 x xs
find2020 x xs =
  case (findSecond x xs) of
    Just y -> y
    Nothing -> find2020 (head xs) (tail xs)

findSecond :: Int -> [Int] -> Maybe Int
findSecond _ [] = Nothing
findSecond x xs
  | sum == 2020 = Just (x * head xs * last xs)
  | sum < 2020 = findSecond x $ tail xs
  | sum > 2020 = findSecond x $ init xs
  where sum = x + head xs + last xs
