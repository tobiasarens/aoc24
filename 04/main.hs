
import Data.Map

data Cell = X | M | A | S | P

data Dir = RGT | LFT | UP | DWN | TR | TL | BR | BL

instance Eq Dir where
    (==) RGT RGT = True
    (==) LFT LFT = True
    (==) UP UP = True
    (==) DWN DWN = True
    (==) TR TR = True
    (==) TL TL = True
    (==) BL BL = True
    (==) BR BR = True
    (==) _ _ = False

instance Show Cell where
    show X = show 'X'
    show M = show 'M'
    show A = show 'A'
    show S = show 'S'
    show P = show '.'

type Grid = Map Int (Map Int Char)


part1 = do
    contents <- readFile "s.txt"
    -- print . map readInt . words $ contents
    let inputs = lines contents
    print inputs

    let grid = parseInput inputs
    print grid
    return ()

toCell :: Char -> Cell
toCell 'X' = X
toCell 'M' = M
toCell 'A' = A
toCell 'S' = S
toCell _ = P

nextChar :: Char -> Char
nextChar 'X' = 'M'
nextChar 'M' = 'A'
nextChar 'A' = 'S'
nextChar _ = '.'

parseLine :: String -> Map Int Char
parseLine [] = empty
parseLine (s:ss) = insert (length ss) (s) (parseLine ss)

parseInput :: [String] -> Grid
parseInput [] = empty
parseInput (s:ss) = insert (length ss) (parseLine s) (parseInput ss)

validIndex :: Grid -> (Int, Int) -> Bool
validIndex grid (x, y) = (member x grid) && (member y (grid ! x))

next :: (Int, Int) -> Dir -> (Int, Int)
next (x, y) RGT = (x+1, y)
next (x, y) LFT = (x-1, y)
next (x, y) UP = (x, y+1)
next (x, y) DWN = (x, y-1)
next (x, y) TL =(x-1, y-1)
next (x, y) TR = (x + 1, y-1)
next (x, y) BR = (x + 1, y + 1)
next (x, y) BL = (x - 1, y + 1)

checkMatch :: Grid -> (Int, Int) -> Dir -> Char -> Bool
checkMatch _ _ _ '.' = False
checkMatch grid (x, y) _ 'S' = (validIndex grid (x,y)) && (grid ! x) ! y == 'S'
checkMatch grid pos dir char = checkMatch grid (next pos dir) dir (nextChar char)

