import Prelude

data CharMatch = Match | NoMatch | OutOfBounds

type Coordinate = (Int, Int)

type IncPair = (Int, Int)

type Count = Int

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

charAtIndex :: Int -> [a] -> Maybe a
charAtIndex i xs
  | (i > -1) && (length xs > i) = Just (xs !! i)
  | otherwise = Nothing

matchesCharInGrid :: Int -> Int -> Char -> [[Char]] -> CharMatch
matchesCharInGrid x y char str = case charAtIndex x str of
  Just line -> case charAtIndex (y `mod` length line) line of
    Just a -> if a == char then Match else NoMatch
    _ -> OutOfBounds
  _ -> OutOfBounds

add :: Coordinate -> Coordinate -> Coordinate
add (x, y) (xInc, yInc) = (x + xInc, y + yInc)

folderb :: (Maybe Coordinate, Coordinate, Count) -> [String] -> Int
folderb (Nothing, inc, count) xs = folderb (Just inc, inc, count) xs
folderb (Just (x, y), inc, count) xs = case matchesCharInGrid x y '#' xs of
  OutOfBounds -> count
  Match -> folderb (Just $ add (x, y) inc, inc, count + 1) xs
  NoMatch -> folderb (Just $ add (x, y) inc, inc, count) xs

fn :: [String] -> Int
fn xs =
  product $
    map
      (\inc -> folderb (Nothing, inc, 0) xs)
      [ (1, 1),
        (1, 3),
        (1, 5),
        (1, 7),
        (2, 1)
      ]
