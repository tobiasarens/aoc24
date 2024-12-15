
data Space a = Value a | Empty 
    deriving (Show, Eq)

type Disk = [(Space Int)]


part1 = do
    contents <- readFile "s.txt"
    let ints = stringToNumber contents
    let disk = parseDiskString ints 1
    print disk

parseDiskString :: [Int] -> Disk
parseDiskString [] _ = []
parseDiskString (xs) number = parseData xs 1

parseData :: [Int] -> Int -> [Space Int]
parseData [] _ = []
parseData (x:xs) num = (fillList x (Value num)) ++ parseFree (xs) num

parseFree :: [Int] -> Int -> Disk
parseFree [] _ = []
parseFree (x:xs) num = (fillList x Empty) ++ parseData xs num 
            

stringToNumber :: String -> [Int]
stringToNumber "" = []
stringToNumber (x:xs) = (toInt x) : stringToNumber xs

fillList :: Int -> (Space a) -> [(Space a)]
fillList 0 _ = []
fillList c val = val : (fillList (c - 1) val)

toInt :: Char -> Int
toInt c = read [c]