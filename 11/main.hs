import Data.Map (Map, empty, alter, keys, union, assocs, map, elems, unionWith)

part1 = do
    contents <- readFile "l.txt"
    let stones = Prelude.map (toInt) $ words contents
    let after = applyN 25 blink stones
    print $ length after

part2 = do
    contents <- readFile "l.txt"
    let stones = Prelude.map (toInt) $ words contents
    let after = applyN 75 blinkMap $ collapse stones
    print $ mapsum after


mapsum :: Map Int Int -> Int
mapsum m = sum $ elems m

blinkMap :: Map Int Int -> Map Int Int
blinkMap m =  foldl unionw empty [(Data.Map.map (snd mm*) . collapse . blink $ [fst mm]) | mm <- assocs m]
    where   
        unionw = unionWith (+)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN i fn a = applyN (i - 1) fn $ fn a

collapse :: [Int] -> Map Int Int
collapse [] = empty
collapse (x:xs) = alter f x $ collapse xs
    where 
        f :: (Maybe Int -> Maybe Int)
        f Nothing = (Just 1)
        f (Just a) = Just $ (1 + a)

blink :: [Int] -> [Int]
blink [] = []
blink (x:xs)
    | x == 0 = 1 : (blink xs)
    | evenDigits $ x = left : right : (blink xs)
    | otherwise = (x * 2024) : (blink xs)
    where
        (left, right) = split x

split :: Int -> (Int, Int)
split x = (toInt lft, toInt rgt)
    where 
        (lft, rgt) = splitS (show x)

splitS :: String -> (String, String)
splitS s = half s


half :: String -> (String, String)
half s = ([ss | ss <- take (div (length s) 2) s ], reverse [ss | ss <- take (div (length s) 2) (reverse s) ])

evenDigits :: Int -> Bool
evenDigits x = evenString $ show x
    where
        evenString x = mod (length x) 2 == 0

toInt :: String -> Int
toInt = read