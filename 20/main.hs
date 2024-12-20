import Data.Map as Map
import Data.List.Split (splitOn)
import Data.PQueue.Prio.Min as PQ
import Data.Set as Set
import Data.Maybe

import Prelude as Pr

data Cell = Free | Wall | Path
    deriving (Eq)

type Pos = (Int, Int)

file = "l.txt"

p1 = do
    contents <- readFile file
    let grid = toGrid $ lines contents
    let start = findInGrid grid 'S'
    let end = findInGrid grid 'E'
    print start
    let path = fromJust $ traverseGrid grid start end
    let cheatable = getCheatableWalls grid
    let zipped = zip cheatable [1..] :: [(Pos, Int)]

    --printGrid id $ markPositions grid cheatable 'X'

    let idx = Pr.foldr (\p m -> Map.insert (fst p) (snd p) m) Map.empty $ zip path [1..]
    --print idx
    let benefits = Pr.map (\c -> (c, calcBenefit idx c)) cheatable
    print benefits
    let condensed =  condense . Pr.map (snd) $ benefits
    print condensed
    print . sum . Pr.map (snd) $ (Pr.filter ((<) 100 . fst) condensed)

p2 = do
    contents <- readFile file
    let grid = toGrid $ lines contents
    let start = findInGrid grid 'S'
    let end = findInGrid grid 'E'
    print start
    let path = fromJust $ traverseGrid grid start end
    let idx = Pr.foldr (\p m -> Map.insert (fst p) (snd p) m) Map.empty $ zip path [1..]

    let b = Pr.map (calcCheating idx) path
    print $ sum b



calcCheating :: Map Pos Int -> Pos -> Int
calcCheating path pos = length . getCheatTargets pos $ path


getCheatTargets :: Pos -> Map Pos Int -> [Pos]
getCheatTargets pos path = Pr.filter (worth pos) . Pr.filter (\p -> manhatten pos p < 20) . Pr.filter (\p -> Map.member p path) . getNeighboursN pos $ 20
    where
        worth pos tgt = 100 < abs (path ! pos - path ! tgt)

getNeighboursN :: Pos -> Int -> [Pos]
getNeighboursN (x, y) n = [(x + i, y+j) | i <- [(-n) .. n], j <- [(-n) .. n]]

calcBenefit :: Map Pos Int -> Pos -> Int
calcBenefit path wall = maxL [diff x y | x <- (neighbours wall), y <- (neighbours wall), x /= y]
    where
        maxL :: Ord a => [a] -> a
        maxL [a] = a
        maxL [a, b] = max a b
        maxL (a:xs) = max a $ maxL (xs)

        diff :: Pos -> Pos -> Int
        diff a b 
            | not $ Map.member a path = -1
            | not $ Map.member b path = -1
            | otherwise = abs (path ! a - path ! b)

isValid :: (Monad m) => Int -> Int -> m Bool
isValid n i = return (i <= n)

condense :: Ord a => [a] -> [(a, Int)]
condense [] = []
condense xs = Map.assocs . Pr.foldl (myUpdate) Map.empty $ xs
    where
        myUpdate :: Ord a => Map a Int -> a -> Map a Int
        myUpdate m a = if Map.member a m 
                        then Map.adjust (+1) a m
                        else Map.insert a 1 m


logRunCheated :: Map Pos Char -> Pos -> Pos -> Int -> [(Pos, Int)] -> IO [Int]
logRunCheated _ _ _ _ [] = return []
logRunCheated grid start end ref ((cheat, i):xs) = do
    let l = runCheated grid start end cheat
    let sub = ((logRunCheated grid start end ref xs) >>= (\ls -> if (ref - l) >= 100 then return (l:ls) else return ls) )
    putStrLn $ show i ++ " done. " ++ (show l)
    sub

runCheated :: Map Pos Char -> Pos -> Pos -> Pos -> Int
runCheated grid start end cheat = length . fromJust $ traverseGrid grid' start end
    where 
        grid' = Map.insert cheat '.' grid

isWorthy :: Map Pos Char -> Pos -> Bool
isWorthy grid wall = (<=) 2 . length . Pr.filter (isFree grid) . getNeighbours grid $ wall
    where
        iShape c = tb c || lr c
        tb (x,y) = all (\x -> isFree grid x) [(x+1, y), (x-1, y)]
        lr (x, y) = tb (y,x)
        isMember g c = Map.member c g
        isFree g c = isMember g c && g ! c /= '#'


getCheatableWalls :: Map Pos Char -> [Pos]
getCheatableWalls grid = Pr.filter (isWorthy grid)  walls
    where
        walls = Map.keys . Map.filter ((==) '#') $ grid

markPositions :: Map Pos Char -> [Pos] -> Char -> Map Pos Char
markPositions grid pos c = Pr.foldl (\m p -> Map.insert p c m) grid pos

traverseGrid :: Map Pos Char -> Pos -> Pos -> Maybe [Pos]
traverseGrid grid start end = traverseGridHelper grid end (PQ.singleton 0 start) Set.empty Map.empty

traverseGridHelper :: Map Pos Char -> Pos -> PQ.MinPQueue Int Pos -> Set.Set Pos -> Map Pos Pos-> Maybe [Pos]
traverseGridHelper grid goal open seen tracks
    | open == PQ.empty = Nothing
    | next == goal = Just $ findPath goal tracks'
    | otherwise = traverseGridHelper grid goal open' seen' tracks'
            where
        (v, next) = PQ.findMin open
        neighbours = Pr.filter (\x -> Set.notMember x seen) . Pr.filter ((/=) '#' . (!) grid ) $ getNeighbours grid next
        open' = Pr.foldl (\q p -> PQ.insert (cost p) p q) (PQ.deleteMin open) neighbours
        seen' = Pr.foldl (\s p -> Set.insert p s) seen (next:neighbours)
        cost p =  v+1 -- + (manhatten goal p)
        tracks' = Pr.foldl (\m p -> Map.insert p next m) tracks neighbours


findInGrid :: Eq a => Map Pos a -> a -> Pos
findInGrid m c = fst .  (\x -> x !! 0). Map.assocs . Map.filter ((==) c) $ m

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


findPath :: Pos -> Map Pos Pos -> [Pos]
findPath pos grid =
    if Map.member pos grid
    then (findPath (grid ! pos) grid ) ++ [pos]
    else [pos]
 
manhatten :: Pos -> Pos -> Int
manhatten (x, y) (a, b) = (abs $ x - a) + (abs $ y - b)

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

getNeighbours :: Map Pos a -> Pos -> [Pos]
getNeighbours grid (x, y) = Pr.filter (\x -> Map.member x grid) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]