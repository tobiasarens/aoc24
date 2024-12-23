import Data.Map as Map
import Data.List as List
import Data.Set as Set
import Data.List.Split (splitOn)
import Prelude as P

p1 = do
    contents <- readFile "l.txt"
    let d = P.foldl (parseLine) Map.empty $ lines contents
    --print d
    let triplets = findTriplets d
    let count = P.foldl (\n s -> n+1) 0 . P.filter viable $ triplets
    print count


p2 = do
    contents <- readFile "l.txt"
    let d = P.foldl (parseLine) Map.empty $ lines contents

    let cliques = findCliques d
    let maxC = sort $ maxL length cliques
    print cliques
    print maxC
    print . concat $ P.map (++[',']) maxC


maxL :: (a -> Int) -> [a] -> a
maxL f [a] = a
maxL f (a:b:xs) = if f left > f right then left else right
    where 
        left = a
        right = (maxL f (b:xs))

findCliques :: Map String [String] -> [[String]]
findCliques m = helper (Map.keys m) []
    where
        helper [] s = s
        helper (a:xs) s = helper remain s'
            where
                cliq = growClique m [a]
                s' = cliq:s
                remain = dropAll cliq xs

dropAll :: Eq a => [a] -> [a] -> [a]
dropAll _ [] = []
dropAll toDrop (x:xs) 
    | contains toDrop x = dropAll toDrop xs
    | otherwise = x : (dropAll toDrop xs)



growClique :: Map String [String] -> [String] -> [String]
growClique m (a:xs) = if P.null con then (a:xs)
                        else growClique m ((con !! 0):a:xs)
    where
        candidates = P.filter (not . contains (a:xs)) (m ! a)
        con = P.filter (\c -> all (connected m c) (a:xs)) candidates



viable :: [String] -> Bool
viable [] = False
viable ((b:bs):as) = b == 't' || viable as

findTuples :: Map String [String] -> [[String]]
findTuples m = [[a, b] | a <- Map.keys m, b <- (m ! a), b /= a, contains (m ! b) a]

findTriplets :: Map String [String] -> [[String]]
findTriplets m = [[a, b, c] | a <- Map.keys m, b <- (m ! a), c <- (m ! b), a < b, b < c, connected m a b, connected m b c, connected m a c]

connected :: Map String [String] -> String -> String -> Bool
connected m a b = contains (m!a) b && contains (m!b) a

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) a = (x==a) || contains xs a

parseLine :: Map String [String] -> String -> Map String [String]
parseLine m line = ins first second . ins second first $ m
    where
        splits = splitOn "-" line
        first = splits !! 0
        second = splits !! 1
        ins :: String -> String -> Map String [String] -> Map String [String]
        ins k v m = if Map.member k m
                    then Map.adjust (v:) k m
                    else Map.insert k [v] m 
