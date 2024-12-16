import Data.Map (Map, empty, keys, (!), insert, union, assocs, alter, adjust)

type Pos = (Int, Int)
data Cell = Wall | Free | Start | End Int | Visited Int
    deriving (Show, Eq)

data Dir = UP | RGT | DWN | LFT | None
    deriving (Show, Enum, Bounded, Eq)
type Grid = Map Pos Cell

data Visit = Visit {
    dir :: Dir,
    position :: Pos,
    value :: Int
} deriving (Show, Eq)

type Tour = [Visit]

p1 = do
    contents <- readFile "s.txt"
    let (grid, (start, end)) = toGrid toCell $ lines contents
    printGrid toChar grid
    print start
    print end
    print $ getDirections grid start

    let v = findPath grid [(Visit RGT start 0)]
    print "starting computation"
    print $ map (value) (fst v)
    printGrid toChar $ snd v


findPath :: Grid -> [Visit] -> ([Visit], Grid)
findPath grid ((Visit d p v):remain) = 
    case (grid ! p) of
        Free -> (run, updatedGrid)
        Start -> (run, updatedGrid)
        (Visited _) -> (run, updatedGrid)
        (End _) -> ([], grid)
        _ -> ([], grid)
    where 
        run = (Visit d p v) : (fst (findPath (updatedGrid) (remain ++ (workOnTile updatedGrid (Visit d p v)))))
        updatedGrid = insert p (Visited v) grid

workOnTile :: Grid -> Visit -> [Visit]
workOnTile grid (Visit origin pos val) = filter reachable [goto d| d <- [UP .. LFT]]
    where 
        reachable :: Visit -> Bool
        reachable v =
            case (grid ! (position v)) of
                Free -> True
                (End _) -> True
                (Visited k) -> False
                _ -> False

        goto :: Dir -> Visit
        goto dir =
            if dir == origin then
                (Visit origin (nextPos pos dir) (val + 1))
            else
                (Visit origin (nextPos pos dir) (val + 1001))


computeStep :: Grid -> Pos -> Dir -> Grid
computeStep grid pos originDir  = helperStep grid pos (grid ! pos) originDir
    where

        helperStep :: Grid -> Pos -> Cell -> Dir -> Grid
        helperStep grid pos c originDir = foldl (\g -> work g (g ! pos)) grid $ getDirections grid pos

        work :: Grid -> Cell -> Dir -> Grid
        work g (Visited i) dir = let nextVal = if dir == originDir then i + 1 else i + 1001 in
            case g ! next of
                Free -> if dir == oppositeDir originDir then (insert next (Visited nextVal) g) else computeStep (insert next (Visited nextVal) g) next dir
                (End k) -> if k > 0 && k < (nextVal) then grid else insert next (End nextVal) g
                _ -> grid
            where 
                next = nextPos pos dir
        work g Start dir = work g (Visited 0) dir
        work g _ _ = g

oppositeDir :: Dir -> Dir
oppositeDir d =
    case d of
        UP -> DWN
        LFT -> RGT
        RGT -> LFT
        DWN -> UP
        _ -> None

getDirections :: Grid -> Pos -> [Dir]
getDirections grid pos = [dir | dir <- [UP .. LFT], isFree dir]
    where 
        isFree :: Dir -> Bool
        isFree dir =
            case grid ! next of
                Free -> True
                Start -> True
                (End _) -> True
                (Visited _) -> True
                _ -> False
            where
                next = nextPos pos dir

nextPos :: Pos -> Dir -> Pos
nextPos (x, y) UP = (x, y - 1)
nextPos (x, y) RGT = (x + 1, y)
nextPos (x, y) DWN = (x, y + 1)
nextPos (x, y) LFT = (x - 1, y)
nextPos pos _ = pos

toCell :: Char -> Cell
toCell '#' = Wall
toCell 'S' = Start
toCell 'E' = End (-1)
toCell _ = Free

toChar :: Cell -> Char
toChar Wall = '#'
toChar Start = 'S'
toChar (End _) = 'E'
toChar Free = '.'
toChar (Visited _) = 'V'

toGrid :: (Char -> Cell) -> [String] -> (Grid, (Pos, Pos))
toGrid fn s = (grid, (findCell Start grid, findCell (End (-1)) grid))
    where
        grid = helper 0 s
        helper :: Int -> [String] -> Grid
        helper _ [] = empty
        helper y (r:rst) = union (helperRow 0 y r) $ helper (y + 1) rst

        helperRow :: Int -> Int -> String -> Grid
        helperRow _ _ "" = empty
        helperRow x y (s:ss) = insert (x, y) (fn s) $ helperRow (x + 1) y ss

        
findCell :: Cell -> Grid -> Pos
findCell cell grid = fst $ (filter ((==) cell . snd) (assocs grid)) !! 0


gridLines :: (a -> Char) -> (Map (Int, Int) a) -> [String]
gridLines toStr m = [[toStr (m ! (x, y)) | x <- [0..mX]] | y <- [0..mY]]
    where
        (mX, mY) = foldl max (0,0) $ keys m


printGrid :: (a -> Char) -> (Map (Int, Int) a) -> IO()
printGrid f grid = do
    let lines = gridLines f grid
    putStr $ unlines lines