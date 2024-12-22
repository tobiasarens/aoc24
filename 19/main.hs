import Data.Set as Set
import Data.List.Split (splitOn)
import Data.List as Ls
import Data.Char (isSpace)

import Prelude as Pr

file = "l.txt"

p1 = do
    contents <- readFile file
    let (options, tasks) = parseContent contents
    --print options
    print tasks
    let possibles = reverse . sortByLength $ Set.elems options
    print possibles
    print tasks
    --let succ = checkAll options tasks 
    let succ = Pr.filter (\x -> fst $ memCheck options x "") tasks
    print $ Pr.foldr (\x -> (+) 1) 0 succ

tmp = do
    contents <- readFile file
    let (options, tasks) = parseContent contents
    let possibles = reverse . sortByLength $ Set.elems options
    print possibles
    let task = tasks !! 0
    print task

bruteForceCheck :: Set String -> String -> Bool
bruteForceCheck possibles task = any (check task) [Set.elems subset | subset <- (Set.elems $ Set.powerSet possibles)]

reduceCheck :: [String] -> String -> Bool
reduceCheck _ "" = True
reduceCheck possibles task = any (\x -> reduceCheck possibles (Pr.drop (length x) task)) valid
    where
        valid = Pr.filter (\x -> prefix x task) (possibles)
        -- optimize: sort possibles in descending size

checkAll :: Set String -> [String] -> [String]
checkAll _ [] = []
checkAll poss (s:ss)    
    | matching = s : (checkAll poss' ss)
    | otherwise = checkAll poss' ss
    where
        (matching, poss') = memCheck poss s ""


memCheck' :: Set String -> String -> String -> Bool
memCheck' possibles inp pref
    | inp == pref = True
    | (length pref > length inp) = False
    | otherwise = any ((==) True) [(memCheck' pos' inp (pref ++ pref')) | pref' <- valid]
        where
            valid = reverse . sortByLength $ Pr.filter (\x -> prefix (pref ++ x) inp) (Set.elems possibles)
            pos' = Pr.foldr (\p s -> Set.insert p s) possibles [(pref ++ v) | v <- valid]

memCheck :: Set String -> String -> String -> (Bool, Set String)
memCheck possibles inp pref
    | inp == pref = (True, possibles)
    | otherwise = (any ((==) True . fst) [(memCheck pos' inp (pref ++ pref')) | pref' <- valid], pos')
        where
            valid = reverse . sortByLength $ Pr.filter (\x -> prefix (pref ++ x) inp) (Set.elems possibles)
            pos' = Pr.foldr (\p s -> Set.insert p s) possibles [(pref ++ v) | v <- valid]

sortByLength :: [String] -> [String]
sortByLength s = sortBy (\a b -> if (length a) > (length b) then GT else LT) s

check :: String -> [String] -> Bool
check task subset = any (task ==) [Pr.foldr (++) "" perm | perm <- (Ls.permutations subset)]

parseContent :: String -> (Set String, [String])
parseContent content = (towels, tasks)
    where
        sp = splitOn [""] $ lines content
        towels = Set.fromList . Pr.map trim $ splitOn "," (sp !! 0 !! 0)
        tasks = sp !! 1

trim :: String -> String
trim = f. f
    where f = reverse . dropWhile isSpace

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix xs [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys