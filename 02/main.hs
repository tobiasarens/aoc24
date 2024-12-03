

main = do
    contents <- readFile "s.txt"
    -- print . map readInt . words $ contents
    let inputs =  map processLine $ lines $ contents
    let result = map isSafe inputs
    print $ sum (map fromEnum result)

part2 = do
    contents <- readFile "l.txt"
    -- print . map readInt . words $ contents
    let inputs =  map processLine $ lines $ contents
    let result = map dampedSafe inputs
    --print result
    print $ sum (map fromEnum result)

processLine :: String -> [Int]
processLine s = map readInt . words $ s

readInt :: String -> Int
readInt = read

incSafe :: [Int] -> Bool
incSafe [] = True
incSafe [x] = True
incSafe (x:y:xs) = y > x && y <= x + 3 && incSafe (y:xs)

decSafe :: [Int] -> Bool
decSafe [] = True
decSafe [x] = True
decSafe (x:y:xs) = y < x && x <= y + 3 && decSafe  (y:xs)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [x] = True
isSafe (x:y:xs)
    | x < y = incSafe (x:y:xs)
    | x > y = decSafe (x:y:xs)
    | otherwise = False

dampedInc :: [Int] -> Bool
dampedInc [] = True
dampedInc [x] = True
dampedInc (x:y:xs) = (incSafe [x, y] && dampedInc (y:xs)) || incSafe (x:xs) || incSafe (y:xs)

dampedDec :: [Int] -> Bool
dampedDec [] = True
dampedDec [x] = True
dampedDec (x:y:xs) = (decSafe [x, y] && dampedDec (y:xs)) || decSafe (x:xs)

dampedSafe :: [Int] -> Bool
dampedSafe xs = dampedInc xs || dampedDec xs
