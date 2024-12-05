import Data.List (sortBy)

data Rule a = Before a a | Any

instance Show a => Show (Rule a) where
    show (Before a b) = show a ++ "|" ++ show b
    show Any = show "any"


part1 = do
    contents <-  readFile "s.txt"
    let lin = lines contents
    let spl = split lin ""
    let rls = map getRule (spl !! 0) :: [Rule String]
    let lsts = map (\x -> splitAll x ',') (spl !! 1) :: [[String]]
    let tmp = filter (\x -> (all (True==)) ( checkList x rls)) lsts
    let middles = map (toInt . getMiddle) tmp
    print $ sum middles

part2 = do
    contents <-  readFile "l.txt"
    let lin = lines contents
    let spl = split lin ""
    let rls = map getRule (spl !! 0) :: [Rule String]
    let lsts = map (\x -> splitAll x ',') (spl !! 1) :: [[String]]
    let tmp = filter (\x -> (any (False==)) ( checkList x rls)) lsts
    let ordered = map (\x -> order x rls) tmp

    let middles = map (toInt . getMiddle) ordered
    print $ sum middles

order :: Eq a => [a] -> [Rule a] -> [a]
order [] _ = []
order lst rls = sortBy (ordering rls) lst
    where 
        ordering ::Eq a =>  [Rule a] -> a -> a -> Ordering
        ordering [] _ _ = EQ
        ordering (Any:rls) a b = ordering rls a b
        ordering ((Before x y): rls) a b
            | x == a && y == b = LT
            | x == b && y == a = GT
            | otherwise = ordering rls a b


toInt :: String -> Int
toInt = read

getMiddle :: [a] -> a
getMiddle [a] = a
getMiddle (x:xs) = getMiddle (dropLast xs)
    where 
        dropLast [x, y] = [x]
        dropLast (x:xs) = x : (dropLast xs)

checkRule :: Eq a => [a] -> Rule a -> Bool
checkRule [] _ = True
checkRule ls Any = True
checkRule (x:xs) (Before a b)
    | not (contains (x:xs) a) = True
    | not (contains (x:xs) b) = True
    | x == a = True
    | x == b = False
    | otherwise = checkRule xs (Before a b)

checkList :: Eq a => [a] -> [Rule a] -> [Bool]
checkList [] rls = [True | r <- rls]
checkList ls rls = map (\rule -> checkRule ls rule) rls

split :: Eq a => [a] -> a -> [[a]]
split [] _ = [[]]
split (x:xs) c 
    | x == c = [[],xs]
    | otherwise = [x: ((split xs c) !! 0), (split xs c) !! 1]

splitAll :: Eq a => [a] -> a -> [[a]]
splitAll [] _ = [[]]
splitAll s c 
    | contains s c = [split s c !! 0] ++ splitAll (split s c !! 1) c
    | otherwise = [s]

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) c
    | x == c = True
    | otherwise = contains xs c

makeRule :: [String] -> Rule String
makeRule [x, y] = Before x y
makeRule _ = Any

getRule :: String -> Rule String
getRule s = makeRule $ split s '|'
