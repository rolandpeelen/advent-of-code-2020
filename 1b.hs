import Data.List
import Prelude

main = do
  input <- getContents
  -- Take the input,
  -- convert it to lines, then read to get ints
  -- then sort the list
  putStr $ show $ find2020 (-1) (sort $ map read $ lines input)

-- This algoritm works with a sorted array. It pins the first item, then 
-- 'findSecond' takes over, which checks wether adding that first item,
-- the head of the rest, and the tail of the list is equal bigger or smaller 
-- than 2020. 
-- If its equals 2020, we've found our hit.
-- If it's smaller, it needs to call itself again with the tail of the list
-- If it's bigger, it needs to call itself again with everything apart form the 
-- tail of the list
-- It works because it's sorted...
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
  | x + head xs + last xs == 2020 = Just (x * head xs * last xs)
  | x + head xs + last xs < 2020 = findSecond x $ tail xs
  | x + head xs + last xs > 2020 = findSecond x $ init xs
