import Data.Sequence (Seq(..), fromList)
import Data.List
import Prelude

main = do
  input <- getContents
  case find2020 $ fromList $ sort $ map read $ lines input of
   Just x -> putStr $ show x
   Nothing -> putStr "Not Found"

find2020 :: Seq Int -> Maybe Int
find2020 Empty = Nothing
find2020 xs
  | sum == 2020 = Just $ head * last
  | sum < 2020 = find2020 tail
  | otherwise = find2020 init
  where
      sum = head + last
      head :<| tail = xs
      init :|> last = xs
