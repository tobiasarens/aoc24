import Data.Map (Map, insert, empty, member, (!), adjust, elems, foldr)

data Dir = UP | DOWN | RIGHT | LEFT

type Grid = Map Int (Map Int Char)

part1 = do
    contents <-  readFile "l.txt"
    let lin = lines contents
    let grid = parseInput lin
    let start = findInGrid grid '^'
    let traversed = Main.traverse start RIGHT grid
    prettyPrint traversed

    let count = sum [countMap m 'X' | m <- elems traversed]
    print count




countMap :: Eq a => Map Int a -> a -> Int
countMap mp c
    | mp == empty = 0
    | otherwise = sum (map (fromEnum . (c ==)) (elems mp))

prettyPrintMap [] = do
    print ""

prettyPrintMap (x:xs) = do
    print (elems x)
    prettyPrintMap xs

prettyPrint graph = do
    prettyPrintMap (elems graph)

findInMap :: Map Int Char -> Char -> Int
findInMap map char
    | map == empty = -1
    | contains (elems map) char = getIndex (elems map) char
    | otherwise = -1

findInGrid :: Grid -> Char -> (Int, Int)
findInGrid grid char 
    | grid == empty = (-1, -1)
    | otherwise = (x, y)
        where 
            findResults = [findInMap m char | m <- (elems grid)]
            y = listMax findResults
            x = getIndex findResults y

listMax :: Ord a => [a] -> a
listMax [a] = a
listMax (x:y:xs) = listMax ((max x y):xs)

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x: xs) a
    | x == a = True
    | otherwise = contains xs a

getIndex :: Eq a => [a] -> a -> Int
getIndex [] _ = 0
getIndex (x:xs) a
    | x == a = 0
    | otherwise = 1 + getIndex xs a

traverse :: (Int, Int) -> Dir -> Grid -> Grid
traverse pos dir grid 
    | not $ validIndex grid pos = grid
    | charAtIs grid (next pos dir) '#' = Main.traverse pos (rightTurn dir) grid
    | otherwise = Main.traverse (next pos dir) dir updatedGrid
                where 
                    (x, y) = pos
                    updatedGrid = adjust (adjust (\x -> 'X') y ) x grid

validIndex :: Grid -> (Int, Int) -> Bool
validIndex grid (x, y) = (member x grid) && (member y (grid ! x))

rightTurn :: Dir -> Dir
rightTurn UP = RIGHT
rightTurn RIGHT = DOWN
rightTurn DOWN = LEFT
rightTurn LEFT = UP

next :: (Int, Int) -> Dir -> (Int, Int)
next (x, y) RIGHT = (x+1, y)
next (x, y) LEFT = (x-1, y)
next (x, y) UP = (x, y+1)
next (x, y) DOWN = (x, y-1)

charAtIs :: Grid -> (Int, Int) -> Char -> Bool
charAtIs grid pos char = charAt grid pos == Just char

charAt :: Grid -> (Int, Int) -> Maybe Char
charAt grid (x, y) 
    | (validIndex grid (x, y)) = Just ((grid ! x) ! y)
    | otherwise = Nothing

parseLine :: String -> Map Int Char
parseLine [] = empty
parseLine "\r" = empty
parseLine (s:ss) = insert (length ss) (s) (parseLine ss)

parseInput :: [String] -> Grid
parseInput [] = empty
parseInput (s:ss) = insert (length ss) (parseLine s) (parseInput ss)