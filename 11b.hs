import Data.List.Index
import qualified Data.Map as M
import Data.Maybe
import Prelude

main = do
  input <- getContents
  putStr $ show $ fn $ lines input

data Direction = NE | N | NW | W | SW | S | SE | E

data Space = Empty | Occupied | Floor deriving (Show, Eq)

type Coordinate = (Int, Int)

type RowId = Int

type ColId = Int

type Row = M.Map ColId Space

type Grid = M.Map RowId Row

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
safeGridLookup (y, x) grid = flatten $ M.lookup x <$> M.lookup y grid

getNextCoordinate :: Direction -> Coordinate -> Coordinate
getNextCoordinate NE (x, y) = (x -1, y -1)
getNextCoordinate N (x, y) = (x, y - 1)
getNextCoordinate NW (x, y) = (x + 1, y - 1)
getNextCoordinate E (x, y) = (x - 1, y)
getNextCoordinate W (x, y) = (x + 1, y)
getNextCoordinate SE (x, y) = (x - 1, y + 1)
getNextCoordinate S (x, y) = (x, y + 1)
getNextCoordinate SW (x, y) = (x + 1, y + 1)

getFirstSeatInDirection :: Grid -> Direction -> Coordinate -> Maybe Coordinate
getFirstSeatInDirection grid d (x, y)
  | t == Just Floor = getFirstSeatInDirection grid d newC
  | isNothing t = Nothing
  | otherwise = Just newC
  where
    newC = getNextCoordinate d (x, y)
    t = safeGridLookup newC grid

generateCombinations :: Grid -> Coordinate -> [Coordinate]
generateCombinations grid c = mapMaybe (\x -> getFirstSeatInDirection grid x c) [NW, N, NE, W, E, SW, S, SE]

countAdjacent :: Coordinate -> Grid -> Int
countAdjacent c grid = length $ filter (Occupied ==) $ mapMaybe (flip safeGridLookup grid) $ generateCombinations grid c

next :: Coordinate -> Space -> Grid -> Space
next (x, y) z grid
  | z == Occupied && adjacents >= 5 = Empty
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
