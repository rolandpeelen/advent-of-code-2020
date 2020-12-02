import Data.Sequence (Seq(..), fromList)
import Data.List
import Prelude

seqHead (x :<| _) = x
seqTail (_ :<| xs) = xs


main = do
  input <- getContents
  case find2020 (-1) ( fromList $ sort $ map read $ lines input) of
   Just x -> putStr $ show x
   Nothing -> putStr "Not Found"

find2020 :: Int -> Seq Int -> Maybe Int
find2020 _ Empty = Nothing 
find2020 (-1) (x :<| xs) = find2020 x xs
find2020 x xs =
  case (findSecond x xs) of
    Just y -> Just y
    Nothing -> find2020 (seqHead xs) (seqTail xs)

findSecond :: Int -> Seq Int -> Maybe Int
findSecond _ Empty = Nothing
findSecond x xs
  | sum == 2020 = Just $ x * head * last
  | sum < 2020 = findSecond x tail
  | otherwise = findSecond x init
  where
      sum = x + head + last
      head :<| tail = xs
      init :|> last = xs
