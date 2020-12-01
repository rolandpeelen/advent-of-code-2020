import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ find2020 $ sort $ map read $ lines input

find2020 :: [Int] -> Int
find2020 [] = -1
find2020 xs
  | sum == 2020 = head xs * last xs
  | sum < 2020 = find2020 $ tail xs
  | sum > 2020 = find2020 $ init xs
  where sum = head xs + last xs 
