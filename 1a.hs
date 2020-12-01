import Data.List
import Prelude

main = do
  input <- getContents -- Take the contents and push that inputs
  -- take input, map it to lines of input, map that with read (so we get Ints)
  -- run that through the find2020, then show (maps it back to String)
  putStr $ show $ find2020 $ (map read $ lines input, [])

-- Find2020 takes two lists, the input, and an intermediate second list, 
-- if it encounters an emtpy list on the left hand side, and we haven't returned
-- something is off and there is no combination of 2020.
-- If we have a list, take the head, check if we can find an item we've already
-- seen with which it makes a pair. If so, return the product, if not, call
-- find2020 again with the head appended to the items we've already seen.
find2020 :: ([Int], [Int]) -> Int
find2020 ([], _) = -1
find2020 (head:xs, ys) =
  case (find (\x -> x == 2020 - head) ys) of
    Just x -> x * head
    Nothing -> find2020 (xs, ys ++ [head])
