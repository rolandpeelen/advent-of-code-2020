import Data.List
import Prelude

main = do
  input <- getContents
  putStr $ show $ find2020 (sort $ map read $ lines input)

find2020 :: [Int] -> Int
find2020 [] = -1
find2020 xs
  | head xs + last xs == 2020 = head xs * last xs
  | head xs + last xs < 2020 = find2020 $ tail xs
  | head xs + last xs > 2020 = find2020 $ init xs
