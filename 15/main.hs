import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, union, assocs, keys, (!))


data Cell = Wall | Robot | Box | Empty | BoxR
    deriving (Show, Eq)

data Direction = UP | RGT | DWN | LFT
    deriving (Show, Eq)

type Grid = Map (Int, Int) Cell
type Pos = (Int, Int)
type Dir = Direction

p1 = do
    contents <- readFile "l.txt"
    --print (contents)
    let (maze, dir) = let s = splitOn [""] (lines contents) in 
                    (s !! 0, s !! 1)
    let directions = map toDirection $ concat dir
    --print directions
    let grid = toGrid maze
    let robotPos = getRobotPos grid
    let final = moveAll directions grid robotPos
    printGrid cellToChar final
    print $ evaluate final

p2 = do
    contents <- readFile "s.txt"
    --print (contents)
    let (maze, dir) = let s = splitOn [""] (lines contents) in 
                    (s !! 0, s !! 1)
    let directions = map toDirection $ concat dir
    --print directions
    let grid = toGrid' maze
    printGrid cellToChar grid


evaluate :: Grid -> Int
evaluate grid = sum [ 100 * y + x | (x, y) <- keys grid, (grid ! (x, y) == Box)]
        

getRobotPos :: Grid -> Pos
getRobotPos grid = fst $ (filter ((==) Robot . snd) (assocs grid)) !! 0

moveAll :: [Direction] -> Grid -> Pos -> Grid
moveAll [] g _ = g
moveAll (d:dd) g pos = moveAll dd g' p'
    where (g', p') = step g pos d

step :: Grid -> Pos -> Direction -> (Grid, Pos)
step grid robot dir = do
    let nextPos = move dir robot
    case (grid ! nextPos) of
        Empty -> (insert nextPos Robot $ insert robot Empty grid, nextPos)
        Wall -> (grid, robot)
        Box -> moveBoxes grid robot dir
        Robot -> (grid, robot)

moveBoxes :: Grid -> Pos -> Dir -> (Grid, Pos)
moveBoxes grid robot dir = do
        let nextPos = (move dir robot)
        if canMove grid nextPos dir then
            (insert robot Empty . insert nextPos Robot $ helperMove grid (move dir robot) dir, nextPos)
        else (grid, robot)

        where
            helperMove :: Grid -> Pos -> Dir -> Grid
            helperMove grid pos dir
                | (grid ! pos) == Empty = insert pos Box grid
                | otherwise = helperMove grid (move dir pos) dir

canMove :: Grid -> Pos -> Dir -> Bool
canMove grid pos dir 
    | (grid ! pos) == Empty = True
    | (grid ! pos) == Wall = False
    | (grid ! pos) == Box = canMove grid (move dir pos) dir
    | (grid ! pos) == BoxR = canMove grid (move dir pos) dir
    | otherwise = False

    

move :: Direction -> (Int, Int) -> (Int, Int)
move UP (x, y) = (x, y - 1)
move RGT (x, y) = (x + 1, y)
move DWN (x, y) = (x, y + 1)
move LFT (x, y) = (x - 1, y)

toDirection :: Char -> Direction
toDirection '>' = RGT
toDirection '<' = LFT
toDirection 'v' = DWN
toDirection 'V' = DWN
toDirection '^' = UP

cellToChar :: Cell -> Char
cellToChar Wall = '#'
cellToChar Robot = '@'
cellToChar Box = 'O'
cellToChar BoxR = ']'
cellToChar Empty = '.'

toCell :: Char -> Cell
toCell '#' = Wall
toCell '@' = Robot
toCell 'O' = Box
toCell _ = Empty

toGrid :: [String] -> Grid
toGrid s = helper 0 s
    where
        helper :: Int -> [String] -> Grid
        helper _ [] = empty
        helper y (r:rst) = union (helperRow 0 y r) $ helper (y + 1) rst

        helperRow :: Int -> Int -> String -> Grid
        helperRow _ _ "" = empty
        helperRow x y (s:ss) = insert (x, y) (toCell s) $ helperRow (x + 1) y ss


toGrid' :: [String] -> Grid
toGrid' s = helper 0 s
    where
        helper :: Int -> [String] -> Grid
        helper _ [] = empty
        helper y (r:rst) = union (helperRow 0 y r) $ helper (y + 1) rst

        helperRow :: Int -> Int -> String -> Grid
        helperRow _ _ "" = empty
        helperRow x y (s:ss) 
            | s == '#' = insert (x, y) Wall . insert (x + 1, y) Wall $ helperRow (x + 2) y ss
            | s == 'O' = insert (x, y) Box . insert (x+1, y) BoxR $ helperRow (x + 2) y ss
            | s == '@' = insert (x, y) Robot . insert (x+1, y) Empty $ helperRow (x + 2) y ss
            | otherwise = insert (x, y) Empty . insert (x+1, y) Empty $ helperRow (x + 2) y ss

gridLines :: (a -> Char) -> (Map (Int, Int) a) -> [String]
gridLines toStr m = [[toStr (m ! (x, y)) | x <- [0..mX]] | y <- [0..mY]]
    where
        (mX, mY) = foldl max (0,0) $ keys m


printGrid :: (a -> Char) -> (Map (Int, Int) a) -> IO()
printGrid f grid = do
    let lines = gridLines f grid
    putStr $ unlines lines