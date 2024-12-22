import Prelude as Pr
import Data.Bits
import Data.Map as Map
import Data.Set as Set


iterations = 2001


p1 = do
    contents <- readFile "l.txt"
    let numbers = Pr.map (read) $ lines contents :: [Int]
    --print numbers
    let final = Pr.map (applyN evolve 2000) numbers
    --print final
    print $ sum final

p2 = do
    contents <- readFile "l.txt"
    let numbers = Pr.map (read) $ lines contents :: [Int]
    print numbers
    --let numbers = [1,2,3,2024]
    let amount = Pr.map getListForNumber numbers
    --print $ amount 
    let d = Pr.map diff amount
    --print d
    let zipped = Pr.map (\(a, b) -> Pr.zip a b) $ Pr.zip (Pr.map (Pr.drop 1) amount) d
    print $ length (zipped!!0)

    let coll = Pr.foldl (addSequences) Map.empty zipped :: Map [Int] Int
    --print coll

    let (seq, v) =  Map.foldrWithKey (updatedMax) ([0], 0) coll
    print (seq, v)

printOcc :: [(Int, Int)] -> [Int] -> IO ()
printOcc [a, b, c] _ = return ()
printOcc ((_,v1):(b,v2):(c,v3):(d,v4):xs) (one:two:three:four:ys) 
    | v1 == one && v2 == two && v3 == three && v4 == four = do
        print d
        printOcc ((b,v2):(c,v3):(d,v4):xs) (one:two:three:four:ys)
    | otherwise = printOcc ((b,v2):(c,v3):(d,v4):xs) (one:two:three:four:ys)

updatedMax :: [Int] -> Int -> ([Int], Int) -> ([Int], Int)
updatedMax k v (mk, mv) = if v > mv then (k, v) else (mk ,mv)

addSequences :: Map [Int] Int -> [(Int, Int)] -> Map [Int] Int
addSequences m zipped = helper m zipped (Set.empty)
        where
            helper :: Map [Int] Int -> [(Int, Int)] -> Set [Int] -> Map [Int] Int
            helper m [] _ = m
            helper m [a] _ = m
            helper m [a, b] _ = m
            helper m [a, b, c] _ = m
            helper m ((_,a):(v2,b):(v3,c):(amount,d):xs) seen =
                    if Set.member seq seen
                    then helper m ((v2,b):(v3,c):(amount,d):xs) seen
                    else 
                        if Map.member seq m
                        then helper (Map.adjust (+amount) seq m) ((v2,b):(v3,c):(amount,d):xs) (Set.insert seq seen)
                        else helper (Map.insert seq amount m) ((v2,b):(v3,c):(amount,d):xs) (Set.insert seq seen)
                where 
                    seq = [a,b,c,d]



getListForNumber :: Int -> [Int]
getListForNumber number = Pr.map lastDigit (helper iterations number)
    where
        helper 0 secret = []
        helper i secret = secret : (helper (i-1) (evolve secret))


diff :: [Int] -> [Int]
diff [] = []
diff [a] = []
diff (a:b:xs) = (b-a):(diff (b:xs))

lastDigit :: Int -> Int
lastDigit = \x -> x `mod` 10

applyN :: (a -> a) -> Int -> a -> a
applyN f n a = Pr.foldl (\a n -> f a) a [1 .. n]

evolve :: Int -> Int
evolve secret = step_3 . step_2 . step_1 $ secret
    where   
        step_1 s = prune . mix s $ s * 64
        step_2 s = prune . mix s $ div s 32
        step_3 s = prune . mix s $ s * 2048

prune :: Int -> Int
prune = \x -> mod x 16777216

mix :: Int -> Int -> Int
mix secret new = xor secret new 
