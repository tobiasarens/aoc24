import Data.List.Split

part1 = do
    contents <- readFile "s.txt"
    -- print . map readInt . words $ contents
    let res = map (splitOn ":") $ lines $ contents
    let f = filter hasSolution' $  map processLine  res
    print f


data Operator = PLUS | MUL
    deriving (Eq, Show, Enum)


calc :: Operator -> Int -> Int -> Int
calc PLUS a b = a + b
calc MUL a b = a * b

hasSolution' :: (Int, [Int]) -> Bool
hasSolution' (a, b) = hasSolution a b

possibilities' :: [Int] -> [Int]
possibilities' [] = [0]
possibilities' [a] = [a]
possibilities' (x:xs) = [x * p | p <- possibilities xs] ++ [x + p | p <- possibilities xs]

possibilities :: [Int] -> [Int]
possibilities s = possibilities' $ reverse s

hasSolution :: Int -> [Int] -> Bool
hasSolution target [] = False
hasSolution target [a] = a == target
hasSolution tgt lst = contains tgt $ possibilities lst


processList :: String -> [Int]
processList s = map toInt (filter (/= "") $ (splitOn " " s))

processLine :: [String] -> (Int, [Int])
processLine [target, numbers] = (toInt target, processList numbers)

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains a (x:xs) = a == x || contains a xs

toInt :: String -> Int
toInt = read

isInt x = x == fromInteger (round x)
