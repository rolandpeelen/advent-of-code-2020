import Data.List.Index
import qualified Data.Map as M
import Data.Maybe
import Prelude

data Space = Empty | Occupied | Floor deriving (Show, Eq)

type Coordinate = (Int, Int)

type RowId = Int

type ColId = Int

type Row = M.Map ColId Space

type Grid = M.Map RowId Row

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

--putStr $ show $ fn $ lines $ input

toSpace :: Char -> Space
toSpace 'L' = Empty
toSpace '#' = Occupied
toSpace '.' = Floor

makeIndexedMap :: [a] -> M.Map Int a
makeIndexedMap xs = M.fromList $ indexed xs

toRow :: String -> Row
toRow = makeIndexedMap . map toSpace

flatten :: Maybe (Maybe a) -> Maybe a
flatten (Just x) = x
flatten _ = Nothing

safeGridLookup :: (Int, Int) -> Grid -> Maybe Space
safeGridLookup (x, y) grid = flatten $ M.lookup x <$> M.lookup y grid

generateCombinations :: Coordinate -> [Coordinate]
generateCombinations (y, x) =
  [ (left, above),
    (x, above),
    (right, above),
    (left, y),
    (right, y),
    (left, below),
    (x, below),
    (right, below)
  ]
  where
    above = y -1
    below = y + 1
    left = x - 1
    right = x + 1

countAdjacent :: Coordinate -> Grid -> Int
countAdjacent c grid = length $ filter (Occupied ==) $ mapMaybe (`safeGridLookup` grid) $ generateCombinations c

next :: Coordinate -> Space -> Grid -> Space
next (x, y) z grid
  | z == Occupied && adjacents >= 4 = Empty
  | z == Empty && adjacents == 0 = Occupied
  | otherwise = z
  where
    adjacents = countAdjacent (x, y) grid

getNextRow :: Grid -> Int -> Row -> Row
getNextRow grid colId = M.mapWithKey (\rowId x -> next (colId, rowId) x grid)

getNextGrid :: Grid -> Grid
getNextGrid grid = M.mapWithKey (getNextRow grid) grid

incr :: Int -> Int
incr = (+ 1)

countSeatsRow :: Row -> Int
countSeatsRow row = length $ filter (Occupied ==) $ map snd $ M.toList row

countSeatsGrid :: Grid -> Int
countSeatsGrid grid = sum $ map (countSeatsRow . snd) $ M.toList grid

countGrids :: Int -> Grid -> Int
countGrids x grid
  | next == grid = countSeatsGrid grid
  | otherwise = countGrids (incr x) next
  where
    next = getNextGrid grid

fn :: [String] -> Int
fn xs = countGrids 0 $ makeIndexedMap $ map toRow xs
