import Data.List
import Prelude

-- interact :: (String -> String) -> IO();
main = do
  contents <- getContents
  putStr (find2020fromInput contents)

find2020 :: ([Int], [Int]) -> Int
find2020 ([], _) = -1
find2020 (head:xs, ys) =
  case (find (\x -> x == 2020 - head) ys) of
    Just x -> x * head
    Nothing -> find2020 (xs, ys ++ [head])

find2020fromInput :: String -> String
find2020fromInput input =
  let allLines = lines input
      intLines = map read allLines
      result = find2020 (intLines, [])
      strResult = show result
   in strResult
