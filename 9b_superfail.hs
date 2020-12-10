import Data.List
import qualified Data.Sequence as S
import Prelude

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Direction = L | R deriving (Show)

type Directions = [Direction]

type Breadcrumbs = [Direction]

seqTail (_ S.:<| xs) = xs

main = do
  input <- getContents
  putStr $ show $ fn $ lines $ input

getRoot :: S.Seq (Tree Int) -> Tree Int
getRoot xs = case S.lookup 0 xs of
  Just x -> x
  Nothing -> Node (-1) Empty Empty

getVal :: Tree Int -> Int
getVal Empty = -1
getVal (Node x _ _) = x

sumNodes :: Tree Int -> Tree Int -> Tree Int
sumNodes (Node x a b) (Node y c d) = case x >= y of
  True -> Node (x + y) (Node x a b) (Node y c d)
  False -> Node (x + y) (Node y c d) (Node x a b)

addLayer :: S.Seq (Tree Int, Tree Int) -> S.Seq (Tree Int)
addLayer xs = foldl (\acc x -> acc S.|> (sumNodes (fst x) (snd x))) S.empty xs

pairs :: S.Seq (Tree Int) -> S.Seq (Tree Int, Tree Int)
pairs xs = S.zip xs (seqTail xs)

createTree :: S.Seq (Tree Int) -> Tree Int
createTree xs = case S.length xs == 1 of
  True -> getRoot xs
  False -> createTree $ addLayer $ pairs xs

mostLeft :: Tree a -> a
mostLeft (Node a Empty _) = a
mostLeft (Node _ l _) = mostLeft l

mostRight :: Tree a -> a
mostRight (Node a _ Empty) = a
mostRight (Node _ _ r) = mostRight r

keepOne :: (Maybe (Tree Int), Maybe (Tree Int)) -> Maybe (Tree Int)
keepOne (Just x, Nothing) = Just x
keepOne (Nothing, Just x) = Just x
keepOne (Just x, Just y) = Just x -- This should go to both actually
keepOne _ = Nothing

findNode :: Int -> Tree Int -> Maybe (Tree Int)
findNode _ Empty = Nothing
findNode x (Node a l r) = case a == x of
  True -> Just (Node a l r)
  False -> keepOne (findNode x l, findNode x r)

findSum :: Tree Int -> Int
findSum (Node _ x y) = mostLeft x + mostRight y

fn xs = fmap findSum $ findNode 31161678 $ createTree $ S.fromList $ map (\x -> (Node x Empty Empty)) $ map (read :: String -> Int) xs
