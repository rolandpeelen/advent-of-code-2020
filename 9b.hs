import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

findMax :: Int -> [Int] -> [Int] -> Maybe Int
findMax _ _ [] = Nothing
findMax x acc (y : ys)
  | rolling == x = Just (maximum acc + minimum acc)
  | rolling < x = findMax x (acc ++ [y]) ys
  | rolling > x = findMax x (drop 1 acc) (y : ys)
  where
    rolling = sum acc

fn xs = findMax 31161678 [] $ map (read :: String -> Int) xs
