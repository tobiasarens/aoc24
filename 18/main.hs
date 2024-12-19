import Data.Map as Map
import Data.List.Split (splitOn)
import Data.PQueue.Prio.Min as PQ
import Data.Set as Set
import Data.Maybe

import Prelude as Pr

data Cell = Free | Wall | Path
    deriving (Eq)

type Pos = (Int, Int)

width = 71
height = 71
dropCount = 1024
start = (0,0)
end = (70,70)

p1 = do
    contents <- readFile "l.txt"
    let wallPositions = Pr.map parsePositions $ lines contents
    --print wallPositions
    let grid = Pr.foldl (\m p -> Map.insert p Wall m) (createGrid width height) $ Pr.take dropCount wallPositions
    --printGrid toChar grid
    let run = fromJust $ traverseGrid grid (0,0) (70,70)
    let grid' = Pr.foldl (\m p -> Map.insert p Path m) (grid) $ run
    --print run
    printGrid toChar grid'
    print $ length run

p2 = do
    contents <- readFile "l.txt"
    let wallPositions = Pr.map parsePositions $ lines contents
    let (grid, remaining) = alterN dropCount (createGrid width height) wallPositions
    printGrid toChar grid
    let block = firstBlocking grid remaining start end
    print block


firstBlocking :: Map Pos Cell -> [Pos] -> Pos -> Pos -> Pos
firstBlocking g [] _ _= (0,0)
firstBlocking g (x:xs) start end
    | Nothing == traverseGrid grid' start end = x
    | otherwise = firstBlocking grid' xs start end
    where
        grid' = Map.insert x Wall g


alterGrid :: Map Pos Cell -> [Pos] -> (Map Pos Cell, [Pos])
alterGrid g [] = (g, [])
alterGrid grid (x:xs) = (inserted, xs)
    where
        inserted = Map.insert x Wall grid

alterN :: Int -> Map Pos Cell -> [Pos] -> (Map Pos Cell, [Pos])
alterN 0 g p = (g, p)
alterN 1 g p = alterGrid g p
alterN i g p = alterN (i-1) g' p'
    where
        (g', p') = alterGrid g p


traverseGrid :: Map Pos Cell -> Pos -> Pos -> Maybe [Pos]
traverseGrid grid start end = traverseGridHelper grid end (PQ.singleton 0 start) Set.empty Map.empty

traverseGridHelper :: Map Pos Cell -> Pos -> PQ.MinPQueue Int Pos -> Set.Set Pos -> Map Pos Pos-> Maybe [Pos]
traverseGridHelper grid goal open seen tracks
    | open == PQ.empty = Nothing
    | next == goal = Just $ findPath goal tracks'
    | otherwise = traverseGridHelper grid goal open' seen' tracks'
            where
        (v, next) = PQ.findMin open
        neighbours = Pr.filter (\x -> Set.notMember x seen) . Pr.filter ((/=) Wall . (!) grid ) $ getNeighbours grid next
        open' = Pr.foldl (\q p -> PQ.insert (cost p) p q) (PQ.deleteMin open) neighbours
        seen' = Pr.foldl (\s p -> Set.insert p s) seen (next:neighbours)
        cost p =  v+1 -- + (manhatten goal p)
        tracks' = Pr.foldl (\m p -> Map.insert p next m) tracks neighbours

findPath :: Pos -> Map Pos Pos -> [Pos]
findPath pos grid =
    if Map.member pos grid
    then (findPath (grid ! pos) grid ) ++ [pos]
    else [pos]
 
manhatten :: Pos -> Pos -> Int
manhatten (x, y) (a, b) = (abs $ x - a) + (abs $ y - b)

getNeighbours :: Map Pos a -> Pos -> [Pos]
getNeighbours grid (x, y) = Pr.filter (\x -> Map.member x grid) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

parsePositions :: String -> Pos
parsePositions s = (toInt x, toInt y)
    where
        sp = splitOn "," s
        (x, y) = (sp !! 0, sp !! 1)

toInt :: String -> Int
toInt = read

createGrid :: Int -> Int -> Map Pos Cell
createGrid sx sy = Pr.foldl (\y x -> Map.insert x Free y) Map.empty [(x, y) | x <- [0 .. sx-1], y <- [0 .. sy-1]] 

toChar :: Cell -> Char
toChar Free = '.'
toChar Wall = '#'
toChar Path = 'O'

popList :: [a] -> [a]
popList [] = []
popList (x:xs) = xs

toGrid :: [String] -> Map Pos Char
toGrid s = grid
    where
        grid = helper 0 s
        helper :: Int -> [String] -> Map (Int, Int) Char
        helper _ [] = Map.empty
        helper y (r:rst) = Map.union (helperRow 0 y r) $ helper (y + 1) rst

        helperRow :: Int -> Int -> String -> Map (Int, Int) Char
        helperRow _ _ "" = Map.empty
        helperRow x y (s:ss) = Map.insert (x, y) (s) $ helperRow (x + 1) y ss


gridLines :: (a -> Char) -> (Map (Int, Int) a) -> [String]
gridLines toStr m = [[toStr (m ! (x, y)) | x <- [0..mX]] | y <- [0..mY]]
    where
        (mX, mY) = Pr.foldl max (0,0) $ Map.keys m


printGrid :: (a -> Char) -> (Map (Int, Int) a) -> IO()
printGrid f grid = do
    let lines = gridLines f grid
    putStr $ unlines lines