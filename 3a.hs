import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | (i > -1) && (length xs > i) = Just (xs !! i)
  | otherwise = Nothing

charMatchAt :: Int -> Char -> [Char] -> Bool
charMatchAt x char str = case safeIndex str x of
  Just y -> y == char
  _ -> False

folder :: (Int, Int, Int) -> [String] -> Int
folder (trees, _, _) [] = trees
folder (trees, runningOffset, offset) (head : xs) =
  if charMatchAt (runningOffset `mod` length head) '#' head
    then folder (trees + 1, runningOffset + offset, offset) xs
    else folder (trees, runningOffset + offset, offset) xs

-- (trees, offset, runningOffset)
fn :: [String] -> Int
fn = folder (0, 0, 3) -- This works because my 0 is a '.' If it was a '#', this wouldn't work.
