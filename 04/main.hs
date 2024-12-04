
import Data.Map

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

type Grid = Map Int (Map Int Char)


part1 = do
    contents <- readFile "l.txt"
    -- print . map readInt . words $ contents
    let inputs = lines contents
    --print inputs

    let grid = parseInput inputs
    --print grid
    let res = [countMatches grid (x, y) | x <- keys grid, y <- keys (grid ! x)]
    --print res
    print $ sum res
    return ()

    
part2 = do
    contents <- readFile "l.txt"
    -- print . map readInt . words $ contents
    let inputs = lines contents
    --print inputs

    let grid = parseInput inputs
    --print grid
    let res = [checkMatch2 grid (x, y) | x <- keys grid, y <- keys (grid ! x)] :: [Bool]
    --print res
    print $ sum $ Prelude.map fromEnum res
    return ()

nextChar :: Char -> Char
nextChar 'X' = 'M'
nextChar 'M' = 'A'
nextChar 'A' = 'S'
nextChar _ = '.'

parseLine :: String -> Map Int Char
parseLine [] = empty
parseLine "\r" = empty
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
checkMatch grid (x, y) dir char = (validIndex grid (x, y)) && (((grid ! x) ! y )== char) && checkMatch grid (next (x, y) dir) dir (nextChar char)

countMatches :: Grid -> (Int, Int) -> Int
countMatches grid pos = sum $ Prelude.map fromEnum [checkMatch grid pos dir 'X' | dir <- [UP, DWN, LFT, RGT, TL, TR, BR, BL]]


checkMatch2 :: Grid -> (Int, Int) -> Bool
checkMatch2 grid pos = (charAtIs grid pos 'A') && ( ((charAtIs grid (next pos TL) 'M') && (charAtIs grid (next pos BR) 'S')) || ((charAtIs grid (next pos TL) 'S') && (charAtIs grid (next pos BR) 'M')) )
                                                && ( ((charAtIs grid (next pos TR) 'M') && (charAtIs grid (next pos BL) 'S')) || ((charAtIs grid (next pos TR) 'S') && (charAtIs grid (next pos BL) 'M')))

charAtIs :: Grid -> (Int, Int) -> Char -> Bool
charAtIs grid pos char = charAt grid pos == Just char

charAt :: Grid -> (Int, Int) -> Maybe Char
charAt grid (x, y) 
    | (validIndex grid (x, y)) = Just ((grid ! x) ! y)
    | otherwise = Nothing