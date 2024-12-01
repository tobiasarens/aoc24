import System.IO
import Control.Monad
import Data.List

comp :: Int -> Int -> Int
comp a b 
    | a < b =  b - a
    | a >= b = a - b

part1 = do
    let list = []
    contents <- readFile "l.txt"
    -- print . map readInt . words $ contents
    let lsts = toLists . map readInt . words $ contents
    -- print lsts
    let sorted = (sort $ fst lsts, sort $ snd lsts)
    let d = diff sorted
    -- print d
    let s = sum d
    print s

part2 = do
    let list = []
    contents <- readFile "l.txt"
    -- print . map readInt . words $ contents
    let lsts = toLists . map readInt . words $ contents
    -- print lsts
    let sorted = (sort $ fst lsts, sort $ snd lsts)
    let c = countAll (fst sorted) (snd sorted)
    print $ sum c



countAll :: [Int] -> [Int] -> [Int]
countAll [] _ = []
countAll (x:xs) l = ((x * (count x l)):(countAll xs l))


count :: Int -> [Int] -> Int
count _ [] = 0
count a (x:xs)
    | a < x = 0
    | a > x = count a xs
    | a == x = subcount (x:xs)
    where 
        subcount :: [Int] -> Int
        subcount [] = 0
        subcount [_] = 1
        subcount (x:y:xs)
            | x == y = 1 + subcount (y:xs)
            | otherwise = 1


toLists :: [Int] -> ([Int], [Int])
toLists [] =  ([], [])
toLists (x:y:xs) = let r = toLists xs 
            in (x:(fst r), y:(snd r))

diff :: ([Int], [Int]) -> [Int]
diff ([], []) = []
diff ((x:xs), (y:ys)) = (comp x y):r
    where r = diff (xs, ys)

readInt :: String -> Int
readInt = read