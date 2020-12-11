import Data.List
import qualified Data.Set as S
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

sumsToSet :: [Int] -> S.Set Int
sumsToSet xs = S.fromList $ [a + b | a <- xs, b <- xs]

findMissing :: S.Set Int -> Int -> Maybe Int
findMissing xs y =
  if S.member y xs
    then Nothing
    else Just y

findAllMissing :: [Int] -> Int
findAllMissing [] = -1
findAllMissing xs = case (\x -> findMissing (sumsToSet $ fst x) (head $ snd x)) $ splitAt 25 xs of
  Nothing -> findAllMissing (drop 1 xs)
  Just x -> x

fn xs = findAllMissing $ map (read :: String -> Int) xs
