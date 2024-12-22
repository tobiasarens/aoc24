import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.PQueue.Prio.Min as PQ
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Prelude as Pr


data NumPad = A | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Show, Eq, Enum, Ord)

data ArrowPad = Acc  | Lft | Up | Rgt | Dwn
    deriving (Show, Eq, Enum, Ord)

data Dir = DUp | DLeft | DRight | DDown

data Graph a = Edges [(a, a, ArrowPad)] deriving (Show)

type NumGraph = Graph NumPad

p1' = do
    contents <- readFile "s.txt"
    print $ lines contents
    let codes = Pr.map (parseCode) $ lines contents
    let numerics = Pr.map (getNum) $ lines contents
    print numerics
    let shortest = Pr.map (shortestForCode numPad) $ codes
    print shortest
    let higher = Pr.map (shortestForCode arrowPad . (:) Acc ) (shortest)
    print higher
    print $ length higher
    let hh = Pr.map (length . shortestForCode arrowPad . (:) Acc) (higher)
    print hh
    let s = sum . Pr.map (\(a, b) -> a * b) $ zip hh numerics
    print s

p1 = do
    contents <- readFile "l.txt"
    print $ lines contents
    let codes = Pr.map (parseCode) $ lines contents
    let numerics = Pr.map (getNum) $ lines contents
    processed <- processCodes codes
    print processed
    print $ sum . Pr.map (\(a, b) -> a * b) $ zip processed numerics

test = do
    let code = [Acc,Lft,Acc,Rgt,Acc,Lft,Dwn,Lft,Acc,Acc,Rgt,Up,Acc,Acc,Rgt,Acc,Dwn,Acc,Acc,Up,Acc,Lft,Dwn,Acc,Acc,Acc,Up,Rgt,Acc]
    let minC = minL length (allWays code)
    print minC
    print . length $ minC

    let code2 = [Acc,Lft,Acc,Rgt,Acc,Dwn,Lft,Lft,Acc,Acc,Rgt,Up,Acc,Acc,Rgt,Acc,Dwn,Acc,Acc,Up,Acc,Lft,Dwn,Acc,Acc,Acc,Up,Rgt,Acc]
    let minC2 = minL length (allWays code2)
    print minC2
    print . length $ minC2

processCodes :: ArrowMapper a => Show a => [[a]] -> IO([Int])
processCodes codes = do
    print "processing"
    print codes
    let lvl1 = Pr.map (allWays) codes
    print (lvl1 !! 4)
    --print $ length ((lvl1 !! 0) !! 0)
    let lvl2 = Pr.map (filterShortest . concat . Pr.map (allWays) . Pr.map ((:) Acc)) (lvl1)
    print $ ((lvl2 !! 4) !! 0)
    print $ length ((lvl2 !! 0) !! 0)
    let lvl3 = Pr.map (filterShortest . concat .  Pr.map (allWays). Pr.map ((:) Acc)) lvl2
    print $ ((lvl3 !! 4) !! 0)
    print $ length ((lvl3 !! 4) !! 0)
    let sLength = Pr.map (\ll -> length (ll !! 0)) lvl3 -- Pr.map (length . minL length) lvl3
    print sLength 
    --print $ length ((lvl3 !! 0) !! 2)
    -- Pr.map (\ll -> length (ll !! 0)) lvl3
    return $ sLength

allWays :: ArrowMapper a => Ord a => [a] -> [[ArrowPad]]
allWays [a] = [[]]
allWays (x:y:xs) = cartesian (Pr.map (mapArrows) pa) (Pr.map ((:) Acc) (allWays (y:xs)))
    where
        pa = shortestPaths (getGraph x) x y


filterShortest :: [[ArrowPad]] -> [[ArrowPad]]
filterShortest pa = Pr.filter (\p -> length p <= maxLength) pa
    where
        maxLength = length $ minL length pa

class Ord a => ArrowMapper a where
    mapArrows :: [a] -> [ArrowPad]
    getGraph :: a -> Graph a

instance ArrowMapper NumPad where
    getGraph a = numPad
    mapArrows :: [NumPad] -> [ArrowPad]
    mapArrows [] = []
    mapArrows [a] = []
    mapArrows (a:b:xs) = dir : (mapArrows (b:xs))
        where
            (Edges edges) = numPad
            (s, e, dir) = (Pr.filter (\(s, e, d) -> s == a && e == b) edges) !! 0

instance ArrowMapper ArrowPad where
    getGraph a = arrowPad
    mapArrows :: [ArrowPad] -> [ArrowPad]
    mapArrows [] = []
    mapArrows [a] = []
    mapArrows (a:b:xs) = dir : (mapArrows (b:xs))
        where
            (Edges edges) = arrowPad
            (s, e, dir) = (Pr.filter (\(s, e, d) -> s == a && e == b) edges) !! 0

shortestPaths :: Ord a => Eq a => Graph a -> a -> a -> [[a]]
shortestPaths graph start end = Pr.filter (\p -> length p <= length minLength) allPossible
    where 
        allPossible = paths graph start end
        minLength = minL length allPossible

paths :: forall a . Ord a => Eq a => Graph a -> a -> a -> [[a]]
paths graph start end = Pr.filter (isValid) $ helper graph start end 5
    where 
        (Edges edges) = graph
        helper :: Graph a -> a -> a -> Int -> [[a]]
        helper graph node end maxLength 
            | node == end = [[end]]
            | maxLength == 0 = []
            | otherwise = Pr.map (\p -> node : p) . Pr.foldl (++) [] $ pathsFromNode
            where
                outgoingEdges = Pr.filter (\(s, e, d) -> s == node) edges
                pathsFromNode = Pr.map (\(s, e, d) -> helper graph e end (maxLength - 1)) outgoingEdges :: [[[a]]]

        isValid l = length l > 0 && (l !! 0) == start && ((reverse l) !! 0) == end

cartesian :: [[a]] -> [[a]] -> [[a]]
cartesian [] ys = ys
cartesian xs [] = xs
cartesian [x] ys = Pr.map (\y -> x ++ y) ys
cartesian (x:xs) ys = Pr.map (\y -> x ++ y) ys ++ cartesian xs ys

minL :: (a -> Int) -> [a] -> a
minL f [a] = a
minL f (a:b:xs) = if f left < f right then left else right
    where 
        left = a
        right = (minL f (b:xs))

shortestForCode :: Ord a => Eq a => Graph a -> [a] -> [ArrowPad]
shortestForCode g [a] = []
shortestForCode g (x:y:xs) = shortest x y ++ [Acc] ++ (shortestForCode g (y:xs))
    where
        shortest x y = dijkstra g x y


getNum :: String -> Int
getNum = read . reverse . Pr.drop 1 . reverse

parseCode :: [Char] -> [NumPad]
parseCode ss = A : (Pr.map (mapC) ss)
    where 
        mapC s = case s of
            '0' -> Zero
            '1' -> One
            '2' -> Two
            '3' -> Three
            '4' -> Four
            '5' -> Five
            '6' -> Six
            '7' -> Seven
            '8' -> Eight
            '9' -> Nine
            'A' -> A
            _ -> A

mirror :: ArrowPad -> ArrowPad
mirror Up = Dwn
mirror Dwn = Up
mirror Rgt = Lft
mirror Lft = Rgt

bidiEdges :: [(a, a, ArrowPad)] -> [(a, a, ArrowPad)]
bidiEdges [] = []
bidiEdges ((a, b, d):xs) = [(a, b, d), (b, a, mirror d)] ++ (bidiEdges xs)


numPad :: Graph NumPad
numPad = Edges (bidiEdges [(A, Zero, Lft), (A, Three, Up), (One, Two, Rgt), (One, Four, Up), (Two, Zero, Dwn), (Two, Five, Up), (Two, Three, Rgt), 
                            (Three, Six, Up), (Four, Seven, Up), (Four, Five, Rgt), (Five, Eight, Up), (Five, Six, Rgt), (Six, Nine, Up),
                            (Seven, Eight, Rgt), (Eight, Nine, Rgt)])

arrowPad :: Graph ArrowPad
arrowPad = Edges (bidiEdges [(Up, Acc, Rgt), (Lft, Dwn, Rgt), (Dwn, Up, Up), (Dwn, Rgt, Rgt), (Rgt, Acc, Up)])

dijkstra :: Eq a => Ord a => Graph a -> a -> a -> [ArrowPad]
dijkstra graph start end = transform graph $ start : (helper graph end (PQ.singleton 0 start) (Set.empty) (Map.empty))
    where
        transform :: Eq a => Graph a -> [a] -> [ArrowPad]
        transform g [] = []
        transform g [a] = []
        transform g (a:b:xs) = dir : (transform g (b:xs))
            where
                (Edges ed) = g
                (s, e, dir) = (Pr.filter (\(s, e, d) -> s == a && e == b) ed) !! 0


        helper :: Eq a => Ord a => Graph a -> a -> MinPQueue Int a -> Set a -> Map a a -> [a]
        helper graph end queue seen pred
            | node == end = makePath end pred
            | Set.member node seen = helper graph end (PQ.drop 1 queue) seen pred
            | otherwise = helper graph end q' seen' pred'
            where
                (v, node) = PQ.findMin queue
                (Edges e) = graph
                neighbours = Pr.filter (\(s, e, d) -> not (Set.member e seen)) $ Pr.filter (\(s, e, d) -> s == node) e
                q' = Pr.foldl (\q (s, e, d) -> PQ.insert (v + 1 + (fromEnum d)) e q) (PQ.deleteMin queue) neighbours
                pred' = Pr.foldl (\m (s, e, d) -> Map.insert e s m) pred neighbours
                seen' = Set.insert node seen

                makePath node pred = if not (Map.member node pred) then [] else makePath (pred ! node) pred ++ [node]
